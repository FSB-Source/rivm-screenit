package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.huisarts;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.cervix.CervixHuisartsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.component.validator.AchternaamValidator;
import nl.rivm.screenit.main.web.component.validator.EmailAddressValidator;
import nl.rivm.screenit.main.web.component.validator.EmailAddressenValidator;
import nl.rivm.screenit.main.web.component.validator.TussenvoegselValidator;
import nl.rivm.screenit.main.web.component.validator.VoorlettersValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.CervixHuisartsPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieBeheer;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.Woonplaats;
import nl.rivm.screenit.model.Woonplaats_;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres_;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie_;
import nl.rivm.screenit.model.cervix.SearchHuisartsLocatieDto;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.organisatie.model.Adres_;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.RangeValidator;
import org.wicketstuff.shiro.ShiroConstraint;

import static nl.rivm.screenit.util.StringUtil.propertyChain;

@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = {
	Recht.GEBRUIKER_HUISARTSENPRAKTIJKEN_BEHEER }, checkScope = true, level = ToegangLevel.INSTELLING, bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX })
public class AanvullendeHaGegevensPage extends OrganisatieBeheer
{
	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private CervixHuisartsService cervixHuisartsService;

	private WebMarkupContainer paspoortPanel;

	private WebMarkupContainer locatiesContainer;

	private WebMarkupContainer buttonContainer;

	private IModel<SearchHuisartsLocatieDto> searchLocatieModel;

	private IModel<CervixHuisarts> huisartsIModel;

	private BootstrapDialog dialog;

	private boolean alleenInzien;

	public AanvullendeHaGegevensPage()
	{
		dialog = new BootstrapDialog("dialog");
		add(dialog);

		CervixHuisarts huisarts = (CervixHuisarts) super.getCurrentSelectedOrganisatie();

		alleenInzien = autorisatieService.getActieVoorOrganisatie(ScreenitSession.get().getLoggedInInstellingGebruiker(), huisarts,
			Recht.GEBRUIKER_HUISARTSENPRAKTIJKEN_BEHEER) == Actie.INZIEN;

		addOrReplacePaspoortPanel(huisarts, null);

		huisartsIModel = ModelUtil.cModel(huisarts);
		setDefaultModel(huisartsIModel);

		searchLocatieModel = Model.of(new SearchHuisartsLocatieDto());

		Form<CervixHuisarts> form = new Form<>("form", huisartsIModel);
		add(form);

		form.add(new Label("agbcode"));
		form.add(new Label("gebruikersnaamHuisartsenPortaal"));
		ComponentHelper.addTextField(form, "email", true, 100, String.class, alleenInzien).add(EmailAddressValidator.getInstance());
		ComponentHelper.addTextField(form, "extraEmails", false, 255, alleenInzien).add(EmailAddressenValidator.getInstance());

		ComponentHelper.addDropDownChoice(form, "organisatieMedewerkers[0].medewerker.aanhef", false, Aanhef.aanhefVormenMedewerkers(), alleenInzien).setNullValid(true);

		ComponentHelper.addTextField(form, "organisatieMedewerkers[0].medewerker.achternaam", true, 50, alleenInzien).add(new AchternaamValidator())
			.setLabel(Model.of("Achternaam"));
		ComponentHelper.addTextField(form, "organisatieMedewerkers[0].medewerker.tussenvoegsel", false, 20, alleenInzien).add(new TussenvoegselValidator());
		ComponentHelper.addTextField(form, "organisatieMedewerkers[0].medewerker.voorletters", false, 20, alleenInzien).add(new VoorlettersValidator());
		ComponentHelper.addTextField(form, "telefoon", false, 25, alleenInzien);

		ComponentHelper.addTextField(form, "postadres.straat", true, 43, String.class, alleenInzien);
		ComponentHelper.addTextField(form, "postadres.huisnummer", true, 10, Integer.class, alleenInzien).add(RangeValidator.minimum(0));
		ComponentHelper.addTextField(form, "postadres.huisnummerToevoeging", false, 26, String.class, alleenInzien);
		ComponentHelper.newPostcodeTextField(form, "postadres.postcode", true, alleenInzien);
		List<Woonplaats> alleWoonplaatsen = hibernateService.loadAll(Woonplaats.class, "naam", true);
		form.add(new ScreenitDropdown<Woonplaats>("postadres.woonplaats", new SimpleListHibernateModel<Woonplaats>(alleWoonplaatsen), new IChoiceRenderer<Woonplaats>()
		{
			@Override
			public Object getDisplayValue(Woonplaats object)
			{
				return (Object) object.getNaam() + " (Gemeente: " + object.getGemeente().getNaam() + ")";
			}

			@Override
			public String getIdValue(Woonplaats object, int index)
			{
				return object.getId().toString();
			}

			@Override
			public Woonplaats getObject(String id, IModel<? extends List<? extends Woonplaats>> choices)
			{
				if (id != null)
				{
					return choices.getObject().stream().filter(w -> w.getId().toString().equals(id)).findFirst().orElse(null);
				}
				return null;
			}
		}).setRequired(true).setEnabled(!alleenInzien));
		locatiesContainer = addLocatieDataTable(huisarts);
		form.add(locatiesContainer);

		form.add(new AjaxLink<Void>("wachtwoordreset")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				CervixHuisarts huisarts = huisartsIModel.getObject();
				cervixHuisartsService.resetWachtwoord(huisarts, ScreenitSession.get().getLoggedInAccount());

				info("Account in het huisartsenportaal wordt gereset.");
			}
		});

		buttonContainer = addButtonsContainer(form);
		form.add(buttonContainer);
	}

	private WebMarkupContainer addButtonsContainer(Form<?> form)
	{
		WebMarkupContainer container = new WebMarkupContainer("buttonContainer");
		container.setOutputMarkupId(true);

		container.add(new AjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				CervixHuisarts huisarts = (CervixHuisarts) form.getModelObject();
				cervixHuisartsService.saveOrUpdateArts(huisarts, LogGebeurtenis.ORGANISATIE_WIJZIG, ScreenitSession.get().getLoggedInInstellingGebruiker());
				addOrReplacePaspoortPanel(huisarts, target);
				info("Huisarts succesvol opgeslagen.");
				BasePage.markeerFormulierenOpgeslagen(target);
			}
		});
		container.add(new AjaxLink<Void>("inactiveren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				CervixHuisarts huisarts = huisartsIModel.getObject();
				cervixHuisartsService.inactiveerHuisarts(huisarts, ScreenitSession.get().getLoggedInInstellingGebruiker());

				addOrReplacePaspoortPanel(huisarts, target);
				info("huisarts succesvol geinactiveerd.");

				WebMarkupContainer container = addButtonsContainer(form);
				buttonContainer.replaceWith(container);
				buttonContainer = container;
				target.add(container);

				container = addLocatieDataTable(huisarts);
				locatiesContainer.replaceWith(container);
				locatiesContainer = container;
				target.add(container);

			}

			@Override
			public boolean isVisible()
			{
				boolean heeftRecht = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_HUISARTSENPRAKTIJKEN_BEHEER, Actie.VERWIJDEREN);
				boolean locatieActief = huisartsIModel.getObject().getActief();
				return heeftRecht && locatieActief;
			}
		});

		return container;
	}

	private void addOrReplacePaspoortPanel(CervixHuisarts huisarts, AjaxRequestTarget target)
	{

		WebMarkupContainer paspoortContainer = new WebMarkupContainer("paspoortContainer");
		paspoortContainer.setOutputMarkupId(true);
		paspoortContainer.add(new CervixHuisartsPaspoortPanel("paspoort", ModelUtil.sModel(huisarts)));
		if (paspoortPanel != null)
		{
			paspoortPanel.replaceWith(paspoortContainer);
		}
		paspoortPanel = paspoortContainer;
		if (target != null)
		{
			target.add(paspoortContainer);
		}
		else
		{
			add(paspoortContainer);
		}
	}

	private WebMarkupContainer addLocatieDataTable(CervixHuisarts huisarts)
	{
		WebMarkupContainer container = new WebMarkupContainer("locatiesContainer");
		container.setOutputMarkupId(true);

		List<IColumn<CervixHuisartsLocatie, String>> columns = new ArrayList<IColumn<CervixHuisartsLocatie, String>>();
		columns.add(new PropertyColumn<>(Model.of("Locatie"), CervixHuisartsLocatie_.NAAM, CervixHuisartsLocatie_.NAAM));
		columns.add(new PropertyColumn<>(Model.of("IBAN"), CervixHuisartsLocatie_.IBAN, CervixHuisartsLocatie_.IBAN));
		columns.add(new PropertyColumn<>(Model.of("Tenaamstelling"), CervixHuisartsLocatie_.IBAN_TENAAMSTELLING, CervixHuisartsLocatie_.IBAN_TENAAMSTELLING));
		columns.add(new PropertyColumn<>(Model.of("Zorgmailklantnummer"), CervixHuisartsLocatie_.ZORGMAILKLANTNUMMER, CervixHuisartsLocatie_.ZORGMAILKLANTNUMMER));
		columns.add(new PropertyColumn<>(Model.of("Straat"), propertyChain(CervixHuisartsLocatie_.LOCATIE_ADRES, CervixHuisartsAdres_.STRAAT),
			propertyChain(CervixHuisartsLocatie_.LOCATIE_ADRES, CervixHuisartsAdres_.STRAAT)));
		columns.add(new AbstractColumn<>(Model.of("Huisnummer+toevoeging"), propertyChain(CervixHuisartsLocatie_.LOCATIE_ADRES, CervixHuisartsAdres_.HUISNUMMER))
		{
			@Override
			public void populateItem(Item<ICellPopulator<CervixHuisartsLocatie>> cellItem, String componentId, IModel<CervixHuisartsLocatie> rowModel)
			{
				CervixHuisartsLocatie locatie = rowModel.getObject();
				if (locatie.getLocatieAdres() != null)
				{
					String tekst = locatie.getLocatieAdres().getHuisnummer().toString();
					if (locatie.getLocatieAdres().getHuisnummerToevoeging() != null)
					{
						tekst += locatie.getLocatieAdres().getHuisnummerToevoeging();
					}
					cellItem.add(new Label(componentId, tekst));
				}
				else
				{
					cellItem.add(new Label(componentId, ""));
				}
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Postcode"), propertyChain(CervixHuisartsLocatie_.LOCATIE_ADRES, CervixHuisartsAdres_.POSTCODE),
			propertyChain(CervixHuisartsLocatie_.LOCATIE_ADRES, Adres_.POSTCODE)));
		columns.add(new PropertyColumn<>(Model.of("Plaats"), propertyChain(CervixHuisartsLocatie_.LOCATIE_ADRES, CervixHuisartsAdres_.WOONPLAATS, Woonplaats_.NAAM),
			propertyChain(CervixHuisartsLocatie_.LOCATIE_ADRES, CervixHuisartsAdres_.WOONPLAATS, Woonplaats_.NAAM)));
		columns.add(new ActiefPropertyColumn<>(Model.of(""), Instelling_.ACTIEF, container, searchLocatieModel));

		ScreenitDataTable<CervixHuisartsLocatie, String> dataTable = new ScreenitDataTable<CervixHuisartsLocatie, String>("locaties", columns,
			new HuisartsLocatieDataProvider(huisartsIModel, searchLocatieModel), 10, Model.of("Locaties"))
		{
			@Override
			protected boolean isRowClickable(IModel<CervixHuisartsLocatie> rowModel)
			{
				return rowModel.getObject().getActief();
			}

			@Override
			public void onClick(AjaxRequestTarget target, IModel<CervixHuisartsLocatie> model)
			{
				dialog.openWith(target, new AanvullendeHaLocatieEditPanel(IDialog.CONTENT_ID, model, alleenInzien)
				{
					@Override
					public void opslaan(AjaxRequestTarget target)
					{
						WebMarkupContainer container = addLocatieDataTable(huisartsIModel.getObject());
						locatiesContainer.replaceWith(container);
						locatiesContainer = container;
						target.add(container);
						dialog.close(target);
						info("Locatie is succesvol opgeslagen");
						BasePage.markeerFormulierenOpgeslagen(target);
					}
				});

			}
		};

		container.add(dataTable);

		return container;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(huisartsIModel);
		ModelUtil.nullSafeDetach(searchLocatieModel);
	}
}
