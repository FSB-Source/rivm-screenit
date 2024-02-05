package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followupradiologie;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.MammaFollowUpInstellingDto;
import nl.rivm.screenit.dto.mamma.MammaFollowUpInstellingRadiologieDto;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.AbstractMammaFollowUpPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.MammaFollowUpGebeldCellPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.MammaFollowUpDoorverwezenFilterOptie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	checkScope = true,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	recht = { Recht.GEBRUIKER_MAMMA_FOLLOW_UP_RADIOLOGIE_WERKLIJST },
	organisatieTypeScopes = { OrganisatieType.SCREENINGSORGANISATIE })
public class MammaFollowUpRadiologieRegioWerklijstPage extends AbstractMammaFollowUpPage
{

	@SpringBean
	private SimplePreferenceService simplePreferenceService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private IModel<List<MammaScreeningsEenheid>> screeningsEenhedenModel = new SimpleListHibernateModel<>(new ArrayList<>());

	private ScreenitListMultipleChoice<MammaScreeningsEenheid> screeningsEenhedenSelector;

	private WebMarkupContainer refreshContainer;

	protected BootstrapDialog dialog;

	private IModel<MammaFollowUpDoorverwezenFilterOptie> doorverwezenFilterOptieModel;

	private IModel<Integer> onderzoekJaar;

	@SpringBean
	private HibernateService hibernateService;

	public MammaFollowUpRadiologieRegioWerklijstPage()
	{
		dialog = new BootstrapDialog("gebeldDialog");
		add(dialog);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		doorverwezenFilterOptieModel = Model.of(MammaFollowUpDoorverwezenFilterOptie.ALLES);
		onderzoekJaar = Model.of();

		MammaFollowUpRadiologieRegioProvider followUpDataRegioProvider = new MammaFollowUpRadiologieRegioProvider(
			ModelUtil.sModel(ScreenitSession.get().getScreeningOrganisatie()), doorverwezenFilterOptieModel, onderzoekJaar);

		refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		List<IColumn<MammaFollowUpInstellingRadiologieDto, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Instelling"), "instellingNaam", "instellingNaam"));
		columns.add(new PropertyColumn<>(
			Model.of("Aantal > " + simplePreferenceService.getString(PreferenceKey.MAMMA_FOLLOW_UP_RADIOLOGIE_WERKLIJST_NA_DOWNLOADEN.name(), "30") + " dagen"),
			"aantalOpenstaande"));
		columns.add(new PropertyColumn<>(Model.of("Telefoon 1"), "telefoon"));
		columns.add(new PropertyColumn<>(Model.of("Telefoon 2"), "telefoon2"));
		columns.add(new PropertyColumn<>(Model.of("Gebeld op"), "laatstGebeld", "laatstGebeld"));
		columns.add(new AbstractColumn<MammaFollowUpInstellingRadiologieDto, String>(Model.of(""))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaFollowUpInstellingRadiologieDto>> item, String id, IModel<MammaFollowUpInstellingRadiologieDto> iModel)
			{
				item.add(new MammaFollowUpGebeldCellPanel(id, dialog, "gebeld")
				{
					@Override
					protected void onOpslaan(AjaxRequestTarget ajaxRequestTarget)
					{
						super.onOpslaan(ajaxRequestTarget);
						MammaFollowUpInstellingDto followUpInstellingDto = iModel.getObject();
						Instelling instelling = hibernateService.get(Instelling.class, followUpInstellingDto.getInstellingId());
						instelling.setMammaRadiologieGebeld(currentDateSupplier.getDate());
						hibernateService.saveOrUpdate(instelling);
						ajaxRequestTarget.add(refreshContainer);
						followUpDataRegioProvider.resetList();
					}
				});
			}
		});

		ScreenitDataTable<MammaFollowUpInstellingRadiologieDto, String> table = new ScreenitDataTable<MammaFollowUpInstellingRadiologieDto, String>("resultaten", columns,
			followUpDataRegioProvider,
			10, Model.of("instelling(en)"))
		{
			@Override
			protected boolean isRowClickable(IModel<MammaFollowUpInstellingRadiologieDto> rowModel)
			{
				return false;
			}
		};

		ScreenitForm zoekForm = new ScreenitForm("zoekForm");

		RadioChoice<MammaFollowUpDoorverwezenFilterOptie> doorverwezenFilter = new RadioChoice<>("doorverwezenFilter", doorverwezenFilterOptieModel,
			Arrays.asList(MammaFollowUpDoorverwezenFilterOptie.values()),
			new EnumChoiceRenderer<>(this));
		doorverwezenFilter.setPrefix("<div class=\"span2\">\n" +
			"<div class=\"control-group\"><label class=\"radio\">");
		doorverwezenFilter.setSuffix("</label></div>\n" +
			"</div>");

		zoekForm.add(doorverwezenFilter);

		IModel<List<Integer>> selecteerbareOnderzoekJaren = Model.ofList(getSelecteerbareOnderzoekJaren());
		ScreenitDropdown<Integer> onderzoekJaarDropdown = new ScreenitDropdown<>("onderzoekJaarDropdown", this.onderzoekJaar, selecteerbareOnderzoekJaren);
		onderzoekJaarDropdown.setNullValid(true);
		zoekForm.add(onderzoekJaarDropdown);

		zoekForm.add(new IndicatingAjaxButton("zoeken")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				followUpDataRegioProvider.resetList();
				target.add(refreshContainer);
			}
		});

		add(zoekForm);

		refreshContainer.add(table);
	}

	private List<Integer> getSelecteerbareOnderzoekJaren()
	{
		List<Integer> selecteerbareOnderzoekJaren = new ArrayList<>();
		int huidigJaar = currentDateSupplier.getLocalDate().getYear();
		for (int selecteerbaarJaar = huidigJaar; selecteerbaarJaar > (huidigJaar - 4); selecteerbaarJaar--)
		{
			selecteerbareOnderzoekJaren.add(selecteerbaarJaar);
		}
		return selecteerbareOnderzoekJaren;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(doorverwezenFilterOptieModel);
	}
}
