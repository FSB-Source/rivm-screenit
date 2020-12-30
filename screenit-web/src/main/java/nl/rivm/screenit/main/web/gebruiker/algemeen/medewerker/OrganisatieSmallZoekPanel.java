package nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitAjaxLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieDataProvider;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class OrganisatieSmallZoekPanel extends GenericPanel<Instelling>
{

	private static final long serialVersionUID = 1L;

	private WebMarkupContainer organisatiesContainer = null;

	@SpringBean
	private AutorisatieService autorisatieService;

	private IModel<Instelling> selectedOrganisatie = Model.of();

	private WebMarkupContainer selectedOrganisatieContainer;

	public OrganisatieSmallZoekPanel(String id)
	{
		super(id);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		InstellingGebruiker loggedInInstellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		ToegangLevel toegangLevel = autorisatieService.getToegangLevel(loggedInInstellingGebruiker, Actie.TOEVOEGEN, true, Recht.GEBRUIKER_MEDEWERKER_ORGANISATIES_BEHEER);
		Instelling searchObject = new Instelling();
		searchObject.setActief(Boolean.TRUE);
		searchObject.add(new Adres());
		if (ToegangLevel.INSTELLING.equals(toegangLevel))
		{
			searchObject.setId(loggedInInstellingGebruiker.getOrganisatie().getId());
		}

		IModel<Instelling> searchObjectModel = ModelUtil.cModel(searchObject);
		setModel(searchObjectModel);
		ScreenitForm<Instelling> organisatieZoekForm = new ScreenitForm<Instelling>("form", searchObjectModel);

		organisatieZoekForm.add(new TextField<String>("naam"));
		organisatieZoekForm.add(new TextField<String>("adressen[0].plaats"));

		List<OrganisatieType> organisatieTypes = autorisatieService.getOrganisatieTypes(loggedInInstellingGebruiker, Actie.TOEVOEGEN, true);
		organisatieTypes.remove(OrganisatieType.HUISARTS);
		ScreenitDropdown<OrganisatieType> soortZorginstelling = new ScreenitDropdown<OrganisatieType>("organisatieType", organisatieTypes);
		soortZorginstelling.setNullValid(true);
		soortZorginstelling.setChoiceRenderer(new EnumChoiceRenderer<OrganisatieType>());
		organisatieZoekForm.add(soortZorginstelling);

		AjaxSubmitLink zoeken = new ScreenitAjaxLink("zoeken")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				organisatiesContainer.setVisible(true);
				organisatiesContainer.addOrReplace(getZoekResultaten());
				target.add(organisatiesContainer);
			}
		};
		organisatieZoekForm.add(zoeken);
		organisatieZoekForm.setDefaultButton(zoeken);

		add(organisatieZoekForm);

		organisatiesContainer = new WebMarkupContainer("organisatiesContainer");
		organisatiesContainer.setOutputMarkupId(true);
		organisatiesContainer.add(getZoekResultaten());
		add(organisatiesContainer);

		selectedOrganisatieContainer = new WebMarkupContainer("selectedOrganisatieContainer");
		selectedOrganisatieContainer.setOutputMarkupId(true);

		addOrReplaceSelectedOrganisatie();
		add(selectedOrganisatieContainer);

		AjaxLink<Object> opslaan = new AjaxLink<Object>("opslaan")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				onCloseWithSelected(target, selectedOrganisatie);
			}

		};
		add(opslaan);
	}

	private void addOrReplaceSelectedOrganisatie()
	{
		Label selectedOrganisatieNaam = new Label("selectedOrganisatieNaam", new PropertyModel<String>(selectedOrganisatie, "naam"));
		boolean hasSelectedOrganisatie = ModelUtil.nullSafeGet(selectedOrganisatie) != null;
		selectedOrganisatieNaam.setVisible(hasSelectedOrganisatie);
		selectedOrganisatieNaam.setOutputMarkupId(true);
		selectedOrganisatieContainer.addOrReplace(selectedOrganisatieNaam);
		WebMarkupContainer geenOrganisatieGeselecteerd = new WebMarkupContainer("geenOrganisatieGeselecteerd");
		geenOrganisatieGeselecteerd.setVisible(!hasSelectedOrganisatie);
		selectedOrganisatieContainer.addOrReplace(geenOrganisatieGeselecteerd);
		AjaxRequestTarget target = getRequestCycle().find(AjaxRequestTarget.class).orElse(null);
		if (target != null)
		{
			target.add(selectedOrganisatieContainer);
		}
	}

	private Component getZoekResultaten()
	{
		Instelling searchObject = getModelObject();

		List<IColumn<Instelling, String>> columns = getOrganisatieColumns(getModel());

		setVorigZoekObject(searchObject);

		ScreenitDataTable<Instelling, String> organisaties = new ScreenitDataTable<Instelling, String>("organisaties", columns,
			new OrganisatieDataProvider(getModel(), null, Arrays.asList(OrganisatieType.HUISARTS), "naam"), new Model<>("organisaties"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<Instelling> model)
			{
				onSelected(target, model);
			}
		};

		return organisaties;
	}

	protected void onSelected(AjaxRequestTarget target, IModel<Instelling> model)
	{
		selectedOrganisatie = model;
		addOrReplaceSelectedOrganisatie();
	}

	protected abstract Instelling getVorigZoekObject();

	protected abstract void setVorigZoekObject(Instelling organisatieSearchObject);

	private List<IColumn<Instelling, String>> getOrganisatieColumns(IModel<Instelling> searchModel)
	{
		List<IColumn<Instelling, String>> columns = new ArrayList<>();

		columns.add(new PropertyColumn<Instelling, String>(Model.of("Naam organisatie"), "naam", "naam"));
		columns.add(new PropertyColumn<Instelling, String>(Model.of("Straat"), "adres.straat", "adressen[0].adres"));
		columns.add(new PropertyColumn<Instelling, String>(Model.of("Plaats"), "adres.plaats", "adressen[0].plaats"));
		columns.add(new PropertyColumn<Instelling, String>(Model.of("Soort organisatie"), "organisatieType", "organisatieType"));
		columns.add(new ActiefPropertyColumn<Instelling, Instelling>(Model.of(""), "actief", organisatiesContainer, searchModel));

		return columns;
	}

	protected abstract void onCloseWithSelected(AjaxRequestTarget target, IModel<Instelling> model);

}
