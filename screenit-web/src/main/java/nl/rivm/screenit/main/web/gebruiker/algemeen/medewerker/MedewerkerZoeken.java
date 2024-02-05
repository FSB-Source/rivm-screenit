package nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker;

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
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import nl.rivm.screenit.main.service.RolService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.NaamChoiceRenderer;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.ZoekenContextMenuItem;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InlogStatus;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.StamtabellenService;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.ListMultipleChoice;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_MEDEWERKER_BEHEER,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
	level = ToegangLevel.EIGEN)
@ZoekenContextMenuItem
public class MedewerkerZoeken extends MedewerkerBeheer
{
	private static final String MEDEWERKER_ZOEKEN_SELECTED_ROLLEN = "MedewerkerZoeken.selectedRollen";

	private static final String MEDEWERKER_ZOEKEN_SELECTED_FUNCTIES = "MedewerkerZoeken.selectedFuncties";

	private static final long serialVersionUID = 1L;

	@SpringBean
	private StamtabellenService stamtabellenService;

	@SpringBean
	private RolService rolService;

	@SpringBean
	private AutorisatieService autorisatieService;

	private final IModel<List<Functie>> selectedFuncties;

	private final IModel<List<Rol>> selectedRollen;

	private Form<Gebruiker> zoekForm;

	public MedewerkerZoeken()
	{
		setCurrentSelectedOrganisatie(null);
		setCurrentSelectedMedewerker(null);
		Gebruiker zoekObject = new Gebruiker();

		InstellingGebruiker loggedInInstellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		zoekObject.setActief(true);
		zoekObject.setOrganisatieMedewerkers(new ArrayList<InstellingGebruiker>());
		zoekObject.getOrganisatieMedewerkers().add(new InstellingGebruiker());
		zoekObject.getOrganisatieMedewerkers().get(0).setOrganisatie(new Instelling());
		zoekObject.getOrganisatieMedewerkers().get(0).getOrganisatie().add(new Adres());
		IModel<Gebruiker> criteriaModel;
		if (ScreenitSession.get().isZoekObjectGezetForComponent(MedewerkerZoeken.class))
		{
			criteriaModel = (IModel<Gebruiker>) ScreenitSession.get().getZoekObject(MedewerkerZoeken.class);
		}
		else
		{
			criteriaModel = Model.of(zoekObject);
		}

		if (ScreenitSession.get().isZoekObjectGezetForComponent(MEDEWERKER_ZOEKEN_SELECTED_FUNCTIES))
		{
			selectedFuncties = (IModel<List<Functie>>) ScreenitSession.get().getZoekObject(MEDEWERKER_ZOEKEN_SELECTED_FUNCTIES);
		}
		else
		{
			selectedFuncties = ModelUtil.listModel(new ArrayList<Functie>());
		}
		if (ScreenitSession.get().isZoekObjectGezetForComponent(MEDEWERKER_ZOEKEN_SELECTED_ROLLEN))
		{
			selectedRollen = (IModel<List<Rol>>) ScreenitSession.get().getZoekObject(MEDEWERKER_ZOEKEN_SELECTED_ROLLEN);
		}
		else
		{
			selectedRollen = ModelUtil.listModel(new ArrayList<Rol>());
		}
		MedewerkerDataProvider medewerkerDataProvider = new MedewerkerDataProvider("achternaam", criteriaModel, selectedFuncties, selectedRollen, true, true);

		final WebMarkupContainer refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		List<IColumn<Gebruiker, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<Gebruiker, String>(Model.of("Naam medewerker"), "achternaam", "naamVolledigMetVoornaam"));
		columns.add(new PropertyColumn<Gebruiker, String>(Model.of("Organisaties"), "instelling")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<Gebruiker>> item, String componentId, IModel<Gebruiker> rowModel)
			{
				Gebruiker medewerker = rowModel.getObject();
				String organisaties = "";

				if (medewerker.getOrganisatieMedewerkers() != null && medewerker.getOrganisatieMedewerkers().size() > 0)
				{
					boolean first = true;
					for (InstellingGebruiker organisatieMedewerker : medewerker.getOrganisatieMedewerkers())
					{
						if (!Boolean.FALSE.equals(organisatieMedewerker.getActief()))
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
		columns.add(new PropertyColumn<Gebruiker, String>(Model.of("Functie"), "functie", "functie.naam"));
		columns.add(new PropertyColumn<Gebruiker, String>(Model.of("Rollen"), "rol")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<Gebruiker>> item, String componentId, IModel<Gebruiker> rowModel)
			{
				Gebruiker medewerker = rowModel.getObject();
				String rollen = "";

				Set<String> rolSet = new HashSet<>();
				if (medewerker.getOrganisatieMedewerkers() != null && medewerker.getOrganisatieMedewerkers().size() > 0)
				{
					for (InstellingGebruiker organisatieMedewerker : medewerker.getOrganisatieMedewerkers())
					{
						if (!Boolean.FALSE.equals(organisatieMedewerker.getActief()))
						{
							for (InstellingGebruikerRol instellinGebruikersRol : organisatieMedewerker.getRollen())
							{
								if (instellinGebruikersRol.isRolActief())
								{
									rolSet.add(instellinGebruikersRol.getRol().getNaam());
								}
							}
						}
					}
				}
				List<String> rolList = new ArrayList<>(rolSet);
				Collections.sort(rolList);
				rollen = StringUtils.join(rolList, ", ");
				item.add(new Label(componentId, rollen));
			}

		});
		columns.add(new PropertyColumn<>(Model.of("Actief tot en met"), "actiefTotEnMet", "actiefTotEnMet"));

		List<Bevolkingsonderzoek> bevolkingsonderzoeken = loggedInInstellingGebruiker.getBevolkingsonderzoeken();

		columns.add(new ActiefPropertyColumn<Gebruiker, Gebruiker>(Model.of(""), "actief", refreshContainer, criteriaModel));

		refreshContainer.add(new ScreenitDataTable<Gebruiker, String>("medewerkers", columns, medewerkerDataProvider, 10, Model.of("medewerkers"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<Gebruiker> model)
			{
				Gebruiker gebruiker = model.getObject();
				setCurrentSelectedMedewerker(gebruiker);
				setResponsePage(new MedewerkerBasisgegevens(ModelUtil.cModel(gebruiker)));
			}

		});

		AjaxLink<Void> toevoegen = new AjaxLink<Void>("medewerkerToevoegen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				Gebruiker medewerker = new Gebruiker();
				medewerker.setActief(true);
				medewerker.setInlogstatus(InlogStatus.OK);
				setResponsePage(new MedewerkerBasisgegevens(ModelUtil.cModel(medewerker)));
			}

		};
		Actie actie = autorisatieService.getActieVoorMedewerker(loggedInInstellingGebruiker, null, Recht.GEBRUIKER_MEDEWERKER_BEHEER);

		toevoegen.setVisible(isMinimumActie(actie, Actie.TOEVOEGEN));
		add(toevoegen);
		setDefaultModel(new CompoundPropertyModel<>(criteriaModel));
		zoekForm = new Form<Gebruiker>("zoekForm", (IModel<Gebruiker>) getDefaultModel());
		add(zoekForm);

		var rollen = rolService.getActieveRollen(bevolkingsonderzoeken);
		rollen.sort(Comparator.comparing(Rol::getNaam));

		zoekForm.add(new TextField<>("achternaam"));
		zoekForm.add(new TextField<>("organisatieMedewerkers[0].organisatie.naam"));
		zoekForm.add(new TextField<>("organisatieMedewerkers[0].organisatie.adressen[0].plaats"));
		zoekForm.add(new TextField<>("uzinummer"));
		zoekForm.add(new TextField<>("emailextra"));
		zoekForm.add(new TextField<>("organisatieMedewerkers[0].organisatie.uziAbonneenummer"));
		zoekForm.add(new ListMultipleChoice<Functie>("functies", new PropertyModel<List<Functie>>(this, "selectedFuncties"),
			ModelUtil.listRModel(stamtabellenService.getFuncties(null), false), new NaamChoiceRenderer<>()));
		zoekForm.add(new ListMultipleChoice<Rol>("rollen", new PropertyModel<List<Rol>>(this, "selectedRollen"),
			ModelUtil.listRModel(rollen, false), new ChoiceRenderer<>("naam")));

		AjaxSubmitLink submitLink = new AjaxSubmitLink("zoeken", zoekForm)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				ScreenitSession.get().setZoekObject(MedewerkerZoeken.class, zoekForm.getModel());
				ScreenitSession.get().setZoekObject(MEDEWERKER_ZOEKEN_SELECTED_ROLLEN, selectedRollen);
				ScreenitSession.get().setZoekObject(MEDEWERKER_ZOEKEN_SELECTED_FUNCTIES, selectedFuncties);
				target.add(refreshContainer);
			}
		};

		zoekForm.add(submitLink);
		zoekForm.setDefaultButton(submitLink);

	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.medewerkers.zoeken", MedewerkerZoeken.class));
		return contextMenuItems;
	}

	public List<Functie> getSelectedFuncties()
	{
		return selectedFuncties.getObject();
	}

	public void setSelectedFuncties(List<Functie> selectedFuncties)
	{
		this.selectedFuncties.setObject(selectedFuncties);
	}

	public List<Rol> getSelectedRollen()
	{
		return selectedRollen.getObject();
	}

	public void setSelectedRollen(List<Rol> selectedRollen)
	{
		this.selectedRollen.setObject(selectedRollen);
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return Boolean.FALSE;
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(selectedFuncties);
		ModelUtil.nullSafeDetach(selectedRollen);
	}
}
