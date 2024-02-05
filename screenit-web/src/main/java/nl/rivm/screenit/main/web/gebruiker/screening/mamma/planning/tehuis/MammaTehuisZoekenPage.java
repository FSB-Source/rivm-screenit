package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.tehuis;

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
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.mamma.MammaBaseTehuisClientenDao;
import nl.rivm.screenit.dao.mamma.MammaBaseTehuisDao;
import nl.rivm.screenit.main.service.mamma.IMammaTehuisDto;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsService;
import nl.rivm.screenit.main.service.mamma.MammaTehuisService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaPlanningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.enums.MammaTehuisSelectie;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_TEHUIS },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaTehuisZoekenPage extends MammaPlanningBasePage
{
	private Form<MammaTehuisFilter> zoekForm;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private MammaBaseStandplaatsService standplaatsService;

	@SpringBean
	private MammaTehuisService tehuisService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private MammaBaseTehuisClientenDao baseTehuisClientenDao;

	@SpringBean
	private MammaBaseTehuisDao baseTehuisDao;

	private final boolean magSoAanpassen;

	public MammaTehuisZoekenPage()
	{
		magSoAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_TEHUIS, Actie.AANPASSEN) && !ingelogdNamensRegio;

		ScreeningOrganisatie ingelogdNamensRegio = ScreenitSession.get().getScreeningOrganisatie();
		IModel<MammaTehuisFilter> criteriaModel;
		if (ScreenitSession.get().isZoekObjectGezetForComponent(MammaTehuisZoekenPage.class))
		{
			criteriaModel = (IModel<MammaTehuisFilter>) ScreenitSession.get().getZoekObject(MammaTehuisZoekenPage.class);
		}
		else
		{
			MammaTehuisFilter zoekObject = new MammaTehuisFilter();
			criteriaModel = new CompoundPropertyModel<>(zoekObject);
			zoekObject = criteriaModel.getObject();
			zoekObject.setRegio(ingelogdNamensRegio);
			zoekObject.setActief(true);
		}

		MammaTehuisDataProvider tehuisDataProvider = new MammaTehuisDataProvider("standplaatsPeriode.vanaf", criteriaModel);

		final WebMarkupContainer refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		List<IColumn<IMammaTehuisDto, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Tehuis"), "tehuis.naam", "tehuis.naam"));
		if (ingelogdNamensRegio == null)
		{
			columns.add(new PropertyColumn<>(Model.of("Screeningsorganisatie"), "tehuis.regio.naam", "tehuis.standplaats.regio.naam"));
		}

		columns.add(new PropertyColumn<>(Model.of("Standplaats"), "tehuis.standplaats.naam", "tehuis.standplaats.naam"));
		columns.add(new DateTimePropertyColumn<>(Model.of("Standplaatsronde vanaf"), "standplaatsPeriode.vanaf", "standplaatsPeriode.vanaf", Constants.getDateFormat()));
		columns.add(new PropertyColumn<IMammaTehuisDto, String>(Model.of("Gekoppeld"), "dossiers.size")
		{
			@Override
			public IModel<String> getDataModel(IModel<IMammaTehuisDto> rowModel)
			{
				long aantalGekoppeldeClienten = baseTehuisClientenDao.countClienten(rowModel.getObject().getTehuis(), MammaTehuisSelectie.GEKOPPELD, null);
				return Model.of(Long.toString(aantalGekoppeldeClienten));
			}
		});
		columns.add(new DateTimePropertyColumn<>(Model.of("Laatste keer uitgenodigd"), "tehuis.uitgenodigd", "tehuis.uitgenodigd", Constants.getDateFormat()));
		columns.add(new PropertyColumn<IMammaTehuisDto, String>(Model.of("Uit te nodigen"), "")
		{
			@Override
			public IModel<String> getDataModel(IModel<IMammaTehuisDto> rowModel)
			{
				long aantalGekoppeldeClienten = baseTehuisClientenDao.countClienten(rowModel.getObject().getTehuis(), MammaTehuisSelectie.GEKOPPELD, null);
				if (aantalGekoppeldeClienten == 0)
				{
					return Model.of("");
				}
				else
				{
					long aantalUitTeNodigenClienten = baseTehuisClientenDao.countClienten(rowModel.getObject().getTehuis(), MammaTehuisSelectie.UIT_TE_NODIGEN, null);
					if (aantalUitTeNodigenClienten > 0)
					{
						return Model.of("Ja");
					}
					else
					{
						return Model.of("Nee");
					}
				}
			}
		});
		columns.add(new ActiefPropertyColumn<>(Model.of(""), "actief", refreshContainer, criteriaModel));

		refreshContainer.add(new ScreenitDataTable<IMammaTehuisDto, String>("resultaten", columns, tehuisDataProvider, 10, Model.of("tehuizen"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<IMammaTehuisDto> model)
			{
				MammaTehuis tehuis = model.getObject().getTehuis();
				MammaStandplaatsRonde standplaatsRonde = baseTehuisDao.getHuidigeStandplaatsRonde(tehuis.getStandplaats());
				if (standplaatsRonde != null)
				{
					setResponsePage(new MammaTehuisEditPage(ModelUtil.cModel(tehuis)));
				}
				else
				{
					info(getString("standplaatsInactiefMelding"));
				}
			}

		});

		AjaxLink<Void> toevoegen = new IndicatingAjaxLink<Void>("tehuisToevoegen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaTehuis tehuis = new MammaTehuis();
				tehuis.setActief(true);
				setResponsePage(new MammaTehuisEditPage(ModelUtil.cModel(tehuis)));
			}
		};

		toevoegen.setVisible(ingelogdNamensRegio != null && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_TEHUIS, Actie.TOEVOEGEN));
		add(toevoegen);
		setDefaultModel(new CompoundPropertyModel<>(criteriaModel));
		zoekForm = new Form<>("zoekForm", (IModel<MammaTehuisFilter>) getDefaultModel());
		add(zoekForm);

		zoekForm.add(new TextField<>("tehuis.naam"));
		ScreenitDropdown<ScreeningOrganisatie> regioComponent = new ScreenitDropdown<>("regio",
			ModelUtil.listRModel(instellingService.getActieveInstellingen(ScreeningOrganisatie.class), false), new ChoiceRenderer<ScreeningOrganisatie>("naam"));
		regioComponent.setVisible(ingelogdNamensRegio == null);
		regioComponent.setNullValid(true);
		zoekForm.add(regioComponent);

		zoekForm.add(maakStandplaatsenDropdown(criteriaModel.getObject().getTehuis().getStandplaats()));

		IndicatingAjaxSubmitLink zoekenButton = new IndicatingAjaxSubmitLink("zoeken", zoekForm)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				ScreenitSession.get().setZoekObject(MammaTehuisZoekenPage.class, zoekForm.getModel());
				target.add(refreshContainer);
			}
		};
		zoekForm.setDefaultButton(zoekenButton);
		zoekForm.add(zoekenButton);
	}

	private ScreenitDropdown<MammaStandplaats> maakStandplaatsenDropdown(MammaStandplaats huidigeStandplaats)
	{
		List<MammaStandplaats> mogelijkeStandplaatsen = getMogelijkeStandplaatsen(huidigeStandplaats);

		ScreenitDropdown<MammaStandplaats> standplaatsenDropdown = new ScreenitDropdown<>("tehuis.standplaats",
			ModelUtil.listRModel(mogelijkeStandplaatsen, false), new ChoiceRenderer<>("naam"));

		standplaatsenDropdown.setNullValid(true);
		standplaatsenDropdown.setEnabled(magAanpassen || magSoAanpassen);

		return standplaatsenDropdown;
	}

	private List<MammaStandplaats> getMogelijkeStandplaatsen(MammaStandplaats huidigeStandplaats)
	{
		List<MammaStandplaats> mogelijkeStandplaatsen;
		if (magSoAanpassen)
		{
			mogelijkeStandplaatsen = standplaatsService.getActieveStandplaatsen(null);
		}
		else
		{
			ScreeningOrganisatie regio = ScreenitSession.get().getScreeningOrganisatie();
			mogelijkeStandplaatsen = standplaatsService.getActieveStandplaatsen(regio);
		}

		return mogelijkeStandplaatsen;
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return Boolean.FALSE;
	}
}
