package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie;

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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.form.PostcodeField;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.ZoekenContextMenuItem;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.Mammapoli;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.RadiologieAfdeling;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.PaLaboratorium;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Session;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.ListMultipleChoice;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_COLOSCOPIECENTRUM_ORG_BEHEER, Recht.GEBRUIKER_MAMMA_MAMMAPOLI_ORG_BEHEER, Recht.GEBRUIKER_MAMMA_RADIOLOGIEAFDELING_ORG_BEHEER,
		Recht.GEBRUIKER_BEHEER_CC_LOCATIES, Recht.GEBRUIKER_BEHEER_CC_GEBIEDEN, Recht.GEBRUIKER_INPAKCENTRUM_ORG_BEHEER, Recht.GEBRUIKER_LABORATORIA_BEHEER,
		Recht.GEBRUIKER_PA_LABORATORIA_BEHEER, Recht.GEBRUIKER_RIVM_BEHEER, Recht.GEBRUIKER_ZORGVERZEKERAARS_BEHEER, Recht.GEBRUIKER_SCREENINGS_ORG_BEHEER,
		Recht.GEBRUIKER_ZORGINSTELLING_ORG_BEHEER, Recht.GEBRUIKER_COLOSCOPIELOCATIE_ORG_BEHEER, Recht.GEBRUIKER_HUISARTSENPRAKTIJKEN_BEHEER,
		Recht.GEBRUIKER_BMHK_LABORATORIA_BEHEER, Recht.GEBRUIKER_CENTRALE_EENHEID_ORG_BEHEER, Recht.GEBRUIKER_BEOORDELINGSEENHEID_ORG_BEHEER,
		Recht.GEBRUIKER_HUISARTSENPRAKTIJKEN_BEHEER },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
	level = ToegangLevel.INSTELLING)
@ZoekenContextMenuItem
public class OrganisatieZoeken extends OrganisatieBeheer
{

	private static final long serialVersionUID = 1L;

	private final IModel<List<OrganisatieType>> selectedOrganisatieTypes;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	private Form<Instelling> zoekForm;

	public OrganisatieZoeken()
	{
		setCurrentSelectedOrganisatie(null);
		setCurrentSelectedMedewerker(null);

		final WebMarkupContainer refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		IModel<Instelling> criteriaModel;
		if (ScreenitSession.get().isZoekObjectGezetForComponent(OrganisatieZoeken.class))
		{
			criteriaModel = (IModel<Instelling>) ScreenitSession.get().getZoekObject(OrganisatieZoeken.class);
		}
		else
		{
			Instelling zoekObject = new Instelling();
			zoekObject.add(new Adres());
			zoekObject.setOrganisatieMedewerkers(new ArrayList<InstellingGebruiker>());
			zoekObject.getOrganisatieMedewerkers().add(new InstellingGebruiker());
			zoekObject.getOrganisatieMedewerkers().get(0).setMedewerker(new Gebruiker());
			zoekObject.setActief(true);
			criteriaModel = Model.of(zoekObject);
		}
		List<OrganisatieType> choices = autorisatieService.getOrganisatieTypes(ScreenitSession.get().getLoggedInInstellingGebruiker(), true);
		if (ScreenitSession.get().isZoekObjectGezetForComponent("OrganisatieZoeken.selectedOrganisatieTypes"))
		{
			selectedOrganisatieTypes = (IModel<List<OrganisatieType>>) ScreenitSession.get().getZoekObject("OrganisatieZoeken.selectedOrganisatieTypes");
		}
		else
		{
			selectedOrganisatieTypes = new ListModel<>(new ArrayList<OrganisatieType>());
		}

		List<IColumn<Instelling, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<Instelling, String>(Model.of("Naam organisatie"), "naam", "naam"));
		columns.add(new PropertyColumn<Instelling, String>(Model.of("Adres"), "adres.straat", "adressen[0].adres")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public IModel<Object> getDataModel(IModel<Instelling> rowModel)
			{
				if (rowModel.getObject().getOrganisatieType() == OrganisatieType.HUISARTS)
				{
					return new PropertyModel<Object>(rowModel, "postadres.adres");
				}
				else
				{
					return (IModel) super.getDataModel(rowModel);
				}
			}

		});
		columns.add(new PropertyColumn<Instelling, String>(Model.of("Plaats"), "adres.plaats", "adressen[0].plaats")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public IModel<Object> getDataModel(IModel<Instelling> rowModel)
			{
				if (rowModel.getObject().getOrganisatieType() == OrganisatieType.HUISARTS)
				{
					return new PropertyModel<Object>(rowModel, "postadres.woonplaats.naam");
				}
				else
				{
					return (IModel) super.getDataModel(rowModel);
				}
			}

		});
		columns.add(new PropertyColumn<Instelling, String>(Model.of("Soort organisatie"), "organisatieType", "organisatieType"));

		columns.add(new ActiefPropertyColumn<Instelling, Instelling>(Model.of(""), "actief", refreshContainer, criteriaModel.getObject()));

		ScreenitDataTable<Instelling, String> organisaties = new ScreenitDataTable<Instelling, String>("organisaties", columns,
			new OrganisatieDataProvider(criteriaModel, selectedOrganisatieTypes, null, "naam"), 10, new Model<>("organisaties"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<Instelling> model)
			{
				Instelling organisatie = model.getObject();
				organisatie.getOrganisatieMedewerkers().size();
				setCurrentSelectedOrganisatie(organisatie);

				BasePage.markeerFormulierenOpgeslagen(target);

				for (GebruikerMenuItem menuItem : OrganisatieBeheer.createContextMenu())
				{
					if (Session.get().getAuthorizationStrategy().isInstantiationAuthorized(menuItem.getTargetPageClass())
						&& !menuItem.getTargetPageClass().equals(OrganisatieZoeken.class))
					{
						setResponsePage(menuItem.getTargetPageClass());
						break;
					}

				}
			}

		};
		refreshContainer.add(organisaties);

		addNieuwOrganisatieDropDown();

		setDefaultModel(new CompoundPropertyModel<>(criteriaModel));
		zoekForm = new Form<Instelling>("zoekForm", (IModel<Instelling>) getDefaultModel());
		add(zoekForm);

		zoekForm.add(new TextField<>("naam"));
		zoekForm.add(new TextField<>("organisatieMedewerkers[0].medewerker.achternaam"));
		zoekForm.add(new TextField<>("adressen[0].plaats"));
		zoekForm.add(new PostcodeField("adressen[0].postcode").setAlleenCijfersToegestaan(true));
		zoekForm.add(new TextField<>("organisatieMedewerkers[0].medewerker.uzinummer"));
		zoekForm.add(new TextField<>("uziAbonneenummer"));
		zoekForm.add(new TextField<>("email"));

		zoekForm.add(new ListMultipleChoice<OrganisatieType>("selectedOrganisatieTypes", new PropertyModel<List<OrganisatieType>>(this, "selectedOrganisatieTypes"), choices,
			new EnumChoiceRenderer<OrganisatieType>()));

		zoekForm.add(new AjaxSubmitLink("zoeken", zoekForm)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				ScreenitSession.get().setZoekObject(OrganisatieZoeken.class, zoekForm.getModel());
				ScreenitSession.get().setZoekObject("OrganisatieZoeken.selectedOrganisatieTypes", selectedOrganisatieTypes);
				target.add(refreshContainer);
			}
		});
	}

	private void addNieuwOrganisatieDropDown()
	{
		List<OrganisatieType> list = autorisatieService.getOrganisatieTypes(ScreenitSession.get().getLoggedInInstellingGebruiker(), Actie.TOEVOEGEN, true);
		list.remove(OrganisatieType.RIVM);
		list.remove(OrganisatieType.HUISARTS);
		ListView<OrganisatieType> nieuwOrganisatieTypes = new ListView<OrganisatieType>("nieuwOrganisatieTypes", list)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(final ListItem<OrganisatieType> item)
			{
				AjaxLink<Void> nieuwOrganisatie = new AjaxLink<Void>("nieuwOrganisatie")
				{

					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						Instelling nieuweOrganisatie = createOrganisatie(item.getModelObject());
						setResponsePage(new OrganisatieBasisgegevens(ModelUtil.cModel(nieuweOrganisatie)));
					}

				};
				item.add(nieuwOrganisatie);

				nieuwOrganisatie.add(new EnumLabel<OrganisatieType>("label", item.getModelObject()));
			}

		};
		add(nieuwOrganisatieTypes);
		Button toevoegen = new Button("toevoegen");
		toevoegen.setVisible(!list.isEmpty());
		add(toevoegen);
	}

	private Instelling createOrganisatie(OrganisatieType organisatieType)
	{
		Instelling nieuweOrganisatie = null;
		Instelling ingelogdVoorOrganisatie = ScreenitSession.get().getLoggedInInstellingGebruiker().getOrganisatie();
		switch (organisatieType)
		{
		case MAMMAPOLI:
			nieuweOrganisatie = new Mammapoli();
			break;
		case RADIOLOGIEAFDELING:
			nieuweOrganisatie = new RadiologieAfdeling();
			break;
		case COLOSCOPIECENTRUM:
			nieuweOrganisatie = new ColoscopieCentrum();
			if (ingelogdVoorOrganisatie.getOrganisatieType().equals(OrganisatieType.ZORGINSTELLING))
			{
				nieuweOrganisatie.setParent(ingelogdVoorOrganisatie);
			}
			break;
		case SCREENINGSORGANISATIE:
			nieuweOrganisatie = new ScreeningOrganisatie();
			((ScreeningOrganisatie) nieuweOrganisatie).setAfspraakDrempelBk(10);
			((ScreeningOrganisatie) nieuweOrganisatie).setFactorMinderValideBk(new BigDecimal(3.0));
			((ScreeningOrganisatie) nieuweOrganisatie).setFactorDubbeleTijdBk(new BigDecimal(2.0));
			((ScreeningOrganisatie) nieuweOrganisatie).setFactorEersteOnderzoekBk(new BigDecimal(1.1));
			break;
		case LABORATORIUM:
			nieuweOrganisatie = new IFobtLaboratorium();
			break;
		case PA_LABORATORIUM:
			nieuweOrganisatie = new PaLaboratorium();
			break;
		case BMHK_LABORATORIUM:
			nieuweOrganisatie = new BMHKLaboratorium();
			break;
		case HUISARTS:
			nieuweOrganisatie = new CervixHuisarts();
			break;
		case ZORGINSTELLING:
			nieuweOrganisatie = new ZorgInstelling();
			if (ingelogdVoorOrganisatie.getOrganisatieType().equals(OrganisatieType.SCREENINGSORGANISATIE))
			{
				nieuweOrganisatie.setParent(ingelogdVoorOrganisatie);
			}
			break;
		case COLOSCOPIELOCATIE:
			nieuweOrganisatie = new ColoscopieLocatie();
			if (ingelogdVoorOrganisatie.getOrganisatieType().equals(OrganisatieType.ZORGINSTELLING))
			{
				nieuweOrganisatie.setParent(ingelogdVoorOrganisatie);
			}
			break;
		case CENTRALE_EENHEID:
			nieuweOrganisatie = new CentraleEenheid();
			if (ingelogdVoorOrganisatie.getOrganisatieType().equals(OrganisatieType.SCREENINGSORGANISATIE))
			{
				nieuweOrganisatie.setRegio(ingelogdVoorOrganisatie);
			}
			break;
		case BEOORDELINGSEENHEID:
			nieuweOrganisatie = new BeoordelingsEenheid();
			break;
		default:
			nieuweOrganisatie = new Instelling();
			break;
		}
		nieuweOrganisatie.setOrganisatieType(organisatieType);
		nieuweOrganisatie.setActief(true);
		return nieuweOrganisatie;
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisaties.zoeken", OrganisatieZoeken.class));
		return contextMenuItems;
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return Boolean.FALSE;
	}

	public List<OrganisatieType> getSelectedOrganisatieTypes()
	{
		return selectedOrganisatieTypes.getObject();
	}

	public void setSelectedOrganisatieTypes(List<OrganisatieType> selectedOrganisatieTypes)
	{
		this.selectedOrganisatieTypes.setObject(selectedOrganisatieTypes);
	}

}
