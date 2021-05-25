package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.aanvragen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;

import nl.rivm.screenit.dao.cervix.CervixHuisartsBaseDao;
import nl.rivm.screenit.main.service.cervix.CervixHuisartsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.CervixHuisartsPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.CervixScreeningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.huisarts.CervixHuisartsOpvraagPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsAanmeldStatus;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierAanvraagStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.RangeValidator;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(constraint = ShiroConstraint.HasPermission, checkScope = true, bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX }, recht = { Recht.UITSTRIJKEND_ARTS,
	Recht.GERBRUIKER_CERVIX_LABFORMULIEREN_AANVRAGEN })
public class CervixLabformulierAanvraagPage extends CervixScreeningBasePage
{
	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private CervixHuisartsService uitstrijkendArtsService;

	@SpringBean
	private CervixHuisartsBaseDao cervixHuisartsBaseDao;

	private WebMarkupContainer uitstrijkendArtsContainer;

	private WebMarkupContainer orderContainer;

	private IModel<CervixHuisarts> huisartsModel;

	private IModel<CervixLabformulierAanvraag> aanvraagModel;

	public CervixLabformulierAanvraagPage()
	{
		InstellingGebruiker ingelogdeGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		Instelling ingelogdeInstelling = ingelogdeGebruiker.getOrganisatie();

		addFilterOptiesVoorScreeningOrganisatie(ingelogdeInstelling);
		if (OrganisatieType.HUISARTS == ingelogdeInstelling.getOrganisatieType())
		{
			huisartsModel = ModelUtil.sModel(hibernateService.load(CervixHuisarts.class, ingelogdeInstelling.getId()));
		}

		uitstrijkendArtsContainer = addUistrijkendArtsContainer();
		add(uitstrijkendArtsContainer);
	}

	private void addFilterOptiesVoorScreeningOrganisatie(Instelling instelling)
	{
		add(new CervixHuisartsOpvraagPanel("huisartsOpvraagPanel", false)
		{
			@Override
			protected void setUitstrijkendArts(AjaxRequestTarget target, CervixHuisarts arts)
			{
				if (arts != null)
				{
					if (arts.getAanmeldStatus() == CervixHuisartsAanmeldStatus.GEREGISTREERD)
					{
						huisartsModel = ModelUtil.sModel(arts);

						WebMarkupContainer container = addUistrijkendArtsContainer();
						uitstrijkendArtsContainer.replaceWith(container);
						uitstrijkendArtsContainer = container;
						target.add(uitstrijkendArtsContainer);
					}
					else
					{
						error(getString("huisarts-niet-geregistreed"));
					}
				}
				else
				{
					error("Geen huisarts gevonden met deze AGB-code.");
				}
			}
		});
	}

	private WebMarkupContainer addUistrijkendArtsContainer()
	{
		WebMarkupContainer uitstrijkendArtsContrainer = new WebMarkupContainer("uitstrijkendHuisartsContainer");
		uitstrijkendArtsContrainer.setOutputMarkupPlaceholderTag(true);
		uitstrijkendArtsContrainer.setVisible(huisartsModel != null);

		Panel panel = new EmptyPanel("paspoort");
		if (huisartsModel != null)
		{
			panel = new CervixHuisartsPaspoortPanel("paspoort", ModelUtil.sModel(huisartsModel.getObject()));
		}
		uitstrijkendArtsContrainer.add(panel);

		aanvraagModel = ModelUtil.cModel(new CervixLabformulierAanvraag());
		Form<CervixLabformulierAanvraag> form = new Form<>("form", aanvraagModel);
		uitstrijkendArtsContrainer.add(form);

		ComponentHelper.addTextField(form, "aantal", true, 2, Integer.class, false).add(RangeValidator.range(10, 75));

		List<CervixHuisartsLocatie> locaties = new ArrayList<CervixHuisartsLocatie>();
		if (huisartsModel != null)
		{
			locaties = cervixHuisartsBaseDao.getActieveHuisartsLocatiesVanHuisarts(huisartsModel.getObject());
		}
		ScreenitDropdown<CervixHuisartsLocatie> locatieDropDown = new ScreenitDropdown<CervixHuisartsLocatie>("huisartsLocatie", ModelUtil.listModel(locaties),
			new IChoiceRenderer<CervixHuisartsLocatie>()
			{
				@Override
				public Object getDisplayValue(CervixHuisartsLocatie object)
				{
					return object.getNaam();
				}

				@Override
				public String getIdValue(CervixHuisartsLocatie object, int index)
				{
					return object.getId().toString();
				}

				@Override
				public CervixHuisartsLocatie getObject(String id, IModel<? extends List<? extends CervixHuisartsLocatie>> choices)
				{
					if (id != null)
					{
						return choices.getObject().stream().filter(hl -> hl.getId().toString().equals(id)).findFirst().orElse(null);
					}
					return null;
				}
			});
		locatieDropDown.setRequired(true);
		locatieDropDown.setNullValid(true);
		form.add(locatieDropDown);

		form.add(new IndicatingAjaxSubmitLink("aanvragen", form)
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				CervixHuisarts huisarts = huisartsModel.getObject();

				InstellingGebruiker igebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();

				uitstrijkendArtsService.aanvraagLabformulieren(aanvraagModel.getObject(), aanvraagModel.getObject().getHuisartsLocatie(), igebruiker);

				WebMarkupContainer container = addLabformulierenAanvraagDataTableContainer();
				orderContainer.replaceWith(container);
				orderContainer = container;
				target.add(orderContainer);
				aanvraagModel.setObject(new CervixLabformulierAanvraag());
				info("U heeft succesvol labformulieren aangevraagd!");
			}
		});

		orderContainer = addLabformulierenAanvraagDataTableContainer();
		uitstrijkendArtsContrainer.add(orderContainer);

		return uitstrijkendArtsContrainer;
	}

	private WebMarkupContainer addLabformulierenAanvraagDataTableContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("aanvraagContainer");
		container.setOutputMarkupId(true);

		List<IColumn<CervixLabformulierAanvraag, String>> columns = new ArrayList<IColumn<CervixLabformulierAanvraag, String>>();
		columns.add(
			new DateTimePropertyColumn<CervixLabformulierAanvraag, String>(Model.of("Aangevraagd op"), "aanvraagDatum", "aanvraagDatum", new SimpleDateFormat("dd-MM-yyyy HH:mm")));
		columns.add(new PropertyColumn<CervixLabformulierAanvraag, String>(Model.of("Aantal"), "aantal", "aantal"));
		columns.add(new EnumPropertyColumn<CervixLabformulierAanvraag, String, CervixLabformulierAanvraagStatus>(Model.of("Status"), "status"));
		columns
			.add(new DateTimePropertyColumn<CervixLabformulierAanvraag, String>(Model.of("Status datum"), "statusDatum", "statusDatum", new SimpleDateFormat("dd-MM-yyyy HH:mm")));
		columns.add(new PropertyColumn<CervixLabformulierAanvraag, String>(Model.of("Aangevraagd door"), "organisatie.naam", "instellingGebruiker.organisatie.naam"));
		columns.add(new PropertyColumn<CervixLabformulierAanvraag, String>(Model.of("Locatie"), "locatie.naam", "huisartsLocatie.naam"));

		ScreenitDataTable<CervixLabformulierAanvraag, String> dataTable = new ScreenitDataTable<CervixLabformulierAanvraag, String>("aanvraagDataTable", columns,
			new CervixLabformulierAanvraagDataProvider(huisartsModel), 10, Model.of("order"))
		{
			@Override
			protected boolean isRowClickable(IModel rowModel)
			{
				return false;
			}
		};
		container.add(dataTable);
		return container;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(huisartsModel);
		ModelUtil.nullSafeDetach(aanvraagModel);
	}
}
