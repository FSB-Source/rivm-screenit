package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.NaamChoiceRenderer;
import nl.rivm.screenit.main.web.component.ScreenitAjaxLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker.MedewerkerDataProvider;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.StamtabellenService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MedewerkerSmallZoekPanel extends GenericPanel<Gebruiker>
{
	private WebMarkupContainer medewerkersContainer = null;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private StamtabellenService stamtabellenService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private IModel<Gebruiker> selectedMedewerker = Model.of();

	private WebMarkupContainer selectedMedewerkerContainer;

	private final Actie minimumActie;

	protected MedewerkerSmallZoekPanel(String id, Actie minimumActie)
	{
		super(id);
		this.minimumActie = minimumActie;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		var loggedInInstellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();

		var toegangLevel = autorisatieService.getToegangLevel(ScreenitSession.get().getLoggedInInstellingGebruiker(), minimumActie, true,
			Recht.GEBRUIKER_MEDEWERKER_BEHEER);
		var searchObject = new Gebruiker();
		searchObject.setActief(Boolean.TRUE);
		if (toegangLevel != null && toegangLevel.equals(ToegangLevel.INSTELLING))
		{
			searchObject.setOrganisatieMedewerkers(new ArrayList<>());
			searchObject.getOrganisatieMedewerkers().add(new InstellingGebruiker());
			searchObject.getOrganisatieMedewerkers().get(0).setOrganisatie(loggedInInstellingGebruiker.getOrganisatie());
		}

		var searchObjectModel = ModelUtil.ccModel(searchObject);
		setModel(searchObjectModel);
		var medewerkerZoekForm = new ScreenitForm<Gebruiker>("form", searchObjectModel);

		medewerkerZoekForm.add(new TextField<String>("achternaam"));
		medewerkerZoekForm.add(new ScreenitDropdown<>("functie", ModelUtil.listRModel(stamtabellenService.getFuncties(null), false), new NaamChoiceRenderer<>()));

		AjaxSubmitLink zoeken = new ScreenitAjaxLink("zoeken")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				medewerkersContainer.setVisible(true);
				medewerkersContainer.addOrReplace(getZoekResultaten());
				target.add(medewerkersContainer);
			}
		};
		medewerkerZoekForm.add(zoeken);
		medewerkerZoekForm.setDefaultButton(zoeken);

		add(medewerkerZoekForm);

		medewerkersContainer = new WebMarkupContainer("medewerkersContainer");
		medewerkersContainer.setOutputMarkupId(true);
		medewerkersContainer.add(getZoekResultaten());
		add(medewerkersContainer);

		selectedMedewerkerContainer = new WebMarkupContainer("selectedMedewerkerContainer");
		selectedMedewerkerContainer.setOutputMarkupId(true);

		addOrReplaceSelectedMedewerker();
		add(selectedMedewerkerContainer);

		var opslaan = new AjaxLink<>("opslaan")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				onCloseWithSelected(target, selectedMedewerker);
			}

		};
		add(opslaan);
	}

	private void addOrReplaceSelectedMedewerker()
	{
		var selectedMedewerkerNaam = new Label("selectedMedewerkerNaam", new PropertyModel<>(selectedMedewerker, "achternaam"));
		var hasSelectedMedewerker = ModelUtil.nullSafeGet(selectedMedewerker) != null;
		selectedMedewerkerNaam.setVisible(hasSelectedMedewerker);
		selectedMedewerkerNaam.setOutputMarkupId(true);
		selectedMedewerkerContainer.addOrReplace(selectedMedewerkerNaam);
		var geenMedewerkerGeselecteerd = new WebMarkupContainer("geenMedewerkerGeselecteerd");
		geenMedewerkerGeselecteerd.setVisible(!hasSelectedMedewerker);
		selectedMedewerkerContainer.addOrReplace(geenMedewerkerGeselecteerd);
		getRequestCycle().find(AjaxRequestTarget.class).ifPresent(target -> target.add(selectedMedewerkerContainer));
	}

	private Component getZoekResultaten()
	{
		var searchObject = getModelObject();

		var columns = getMedewerkerColumns(getModel());

		setVorigZoekObject(searchObject);

		return new ScreenitDataTable<>("medewerkers", columns,
			new MedewerkerDataProvider("achternaam", getModel()), new Model<>("medewerkers"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<Gebruiker> model)
			{
				onSelected(target, model);
			}
		};
	}

	protected void onSelected(AjaxRequestTarget target, IModel<Gebruiker> model)
	{
		selectedMedewerker = model;
		addOrReplaceSelectedMedewerker();
	}

	protected abstract Gebruiker getVorigZoekObject();

	protected abstract void setVorigZoekObject(Gebruiker searchObject);

	private List<IColumn<Gebruiker, String>> getMedewerkerColumns(IModel<Gebruiker> searchModel)
	{
		List<IColumn<Gebruiker, String>> columns = new ArrayList<>();

		columns.add(new PropertyColumn<>(Model.of("Naam medewerker"), "achternaam", "naamVolledigMetVoornaam"));
		columns.add(new PropertyColumn<>(Model.of("Organisaties"), "instelling")
		{
			@Override
			public void populateItem(Item<ICellPopulator<Gebruiker>> item, String componentId, IModel<Gebruiker> rowModel)
			{
				var medewerker = rowModel.getObject();
				var organisaties = "";

				if (medewerker.getOrganisatieMedewerkers() != null && !medewerker.getOrganisatieMedewerkers().isEmpty())
				{
					var first = true;
					for (var organisatieMedewerker : medewerker.getOrganisatieMedewerkers())
					{
						boolean inDienst = Boolean.FALSE;
						for (var rol : organisatieMedewerker.getRollen())
						{
							if (rol.getActief() && (rol.getEindDatum() == null || DateUtil.compareAfter(rol.getEindDatum(), currentDateSupplier.getDate())))
							{
								inDienst = Boolean.TRUE;
								break;
							}
						}

						if (inDienst && !Boolean.FALSE.equals(organisatieMedewerker.getActief()))
						{
							if (!first)
							{
								organisaties += ", ";
							}
							organisaties += organisatieMedewerker.getOrganisatie().getNaam();
							first = false;
						}
					}
				}
				item.add(new Label(componentId, organisaties));
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Functie"), "functie", "functie.naam"));
		columns.add(new ActiefPropertyColumn<>(Model.of(""), "actief", medewerkersContainer, searchModel));

		return columns;
	}

	protected abstract void onCloseWithSelected(AjaxRequestTarget target, IModel<Gebruiker> model);

}
