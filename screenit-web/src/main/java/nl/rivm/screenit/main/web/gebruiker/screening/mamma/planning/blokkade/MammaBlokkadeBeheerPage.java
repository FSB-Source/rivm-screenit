package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.blokkade;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaPlanningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaBlokkade_;
import nl.rivm.screenit.model.mamma.enums.MammaBlokkadeType;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaBlokkadeBeheerPage extends MammaPlanningBasePage
{
	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	@SpringBean
	private MammaBaseStandplaatsService standplaatsService;

	private BootstrapDialog dialog;

	public MammaBlokkadeBeheerPage()
	{
		dialog = new BootstrapDialog("dialog");
		add(dialog);

		var sessionSO = ScreenitSession.get().getScreeningOrganisatie();

		IModel<MammaBlokkade> zoekObjectModel;
		if (ScreenitSession.get().isZoekObjectGezetForComponent(MammaBlokkadeBeheerPage.class))
		{
			zoekObjectModel = (IModel<MammaBlokkade>) ScreenitSession.get().getZoekObject(MammaBlokkadeBeheerPage.class);
		}
		else
		{
			var zoekObject = new MammaBlokkade();
			zoekObject.setActief(true);
			zoekObject.setRegio(sessionSO);
			zoekObjectModel = ModelUtil.cModel(zoekObject);
			ScreenitSession.get().setZoekObject(MammaBlokkadeBeheerPage.class, zoekObjectModel);
		}
		setDefaultModel(zoekObjectModel);

		var form = new Form<MammaBlokkade>("form");
		add(form);

		var regio = new ScreenitDropdown<>("regio",
			ModelUtil.listRModel(instellingService.getActieveInstellingen(ScreeningOrganisatie.class), true), new ChoiceRenderer<>("naam"));
		regio.setVisible(sessionSO == null);
		regio.setNullValid(true);
		form.add(regio);

		var screeningsEenheid = new ScreenitDropdown<>("screeningsEenheid",
			ModelUtil.listRModel(screeningsEenheidService.getActieveScreeningsEenhedenVoorScreeningOrganisatie(sessionSO)), new ChoiceRenderer<>("naam"));
		screeningsEenheid.setNullValid(true);
		form.add(screeningsEenheid);

		var standplaats = new ScreenitDropdown<>("standplaats",
			ModelUtil.listRModel(standplaatsService.getActieveStandplaatsen(sessionSO)), new ChoiceRenderer<>("naam"));
		standplaats.setNullValid(true);
		form.add(standplaats);

		var type = new ScreenitDropdown<>("type", Arrays.asList(MammaBlokkadeType.values()), new EnumChoiceRenderer<>());
		type.setNullValid(true);
		form.add(type);

		var vanaf = ComponentHelper.newYearDatePicker("vanaf");
		form.add(vanaf);

		var totEnMet = ComponentHelper.newYearDatePicker("totEnMet");
		form.add(totEnMet);

		form.add(new DependantDateValidator(vanaf, totEnMet, DependantDateValidator.Operator.AFTER));

		var blokkadesContainer = new WebMarkupContainer("blokkadesContainer");
		blokkadesContainer.setOutputMarkupId(true);
		add(blokkadesContainer);

		List<IColumn<MammaBlokkade, String>> columns = new ArrayList<>();
		columns.add(new EnumPropertyColumn<>(Model.of("Type"), "type", "type"));
		columns.add(new AbstractColumn<>(Model.of("Blokkade"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaBlokkade>> cell, String id, IModel<MammaBlokkade> blokkadeModel)
			{
				var blokkade = blokkadeModel.getObject();
				String label = null;
				switch (blokkade.getType())
				{
				case SCREENINGS_ORGANISATIE:
					label = blokkade.getRegio().getNaam();
					break;
				case SCREENINGS_EENHEID:
					label = blokkade.getScreeningsEenheid().getNaam();
					break;
				case STANDPLAATS:
					label = blokkade.getStandplaats().getNaam();
					break;
				}
				cell.add(new Label(id, label));
			}
		});
		var format = new SimpleDateFormat("dd-MM-yyyy");
		columns.add(new DateTimePropertyColumn<>(Model.of("Vanaf"), "vanaf", MammaBlokkade_.VANAF, format));
		columns.add(new DateTimePropertyColumn<>(Model.of("Tot en met"), "totEnMet", MammaBlokkade_.TOT_EN_MET, format));
		columns.add(new ActiefPropertyColumn<>(Model.of(""), "actief", blokkadesContainer, zoekObjectModel));

		var blokkadeZoeken = new AjaxSubmitLink("blokkadeZoeken")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				target.add(blokkadesContainer);
				ScreenitSession.get().setZoekObject(MammaBlokkadeBeheerPage.class, zoekObjectModel);

				setResponsePage(MammaBlokkadeBeheerPage.class);
			}
		};
		form.setDefaultButton(blokkadeZoeken);

		form.add(blokkadeZoeken);

		blokkadesContainer.add(new ScreenitDataTable<>("blokkades", columns, new MammaBlokkadeProvider(zoekObjectModel), 10, Model.of("blokkade(s)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaBlokkade> blokkadeModel)
			{
				dialog.openWith(target, new MammaBlokkadeEditPanel(IDialog.CONTENT_ID, blokkadeModel, null)
				{
					private static final long serialVersionUID = 1L;

					@Override
					protected void blokkadeGewijzigd(AjaxRequestTarget target)
					{
						close(target);
						target.add(blokkadesContainer);
					}

					@Override
					protected void close(AjaxRequestTarget target)
					{
						dialog.close(target);
					}
				});
			}
		});

		AjaxLink<Void> blokkade = new AjaxLink<>("blokkade")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.openWith(target, new MammaBlokkadeEditPanel(IDialog.CONTENT_ID, null, null)
				{
					private static final long serialVersionUID = 1L;

					@Override
					protected void blokkadeGewijzigd(AjaxRequestTarget target)
					{
						close(target);
						target.add(blokkadesContainer);
					}

					@Override
					protected void close(AjaxRequestTarget target)
					{
						dialog.close(target);
					}
				});
			}
		};
		blokkade.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.AANPASSEN) && sessionSO != null);
		add(blokkade);
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return Boolean.FALSE;
	}

}
