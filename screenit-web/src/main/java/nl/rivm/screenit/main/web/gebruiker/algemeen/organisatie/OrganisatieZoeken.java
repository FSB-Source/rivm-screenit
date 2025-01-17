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
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.form.PostcodeField;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.ZoekenContextMenuItem;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.util.OrganisatieUtil;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.organisatie.model.Adres_;
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

import static nl.rivm.screenit.model.Instelling_.ACTIEF;
import static nl.rivm.screenit.model.Instelling_.ADRESSEN;
import static nl.rivm.screenit.model.Instelling_.NAAM;
import static nl.rivm.screenit.model.Instelling_.ORGANISATIE_TYPE;
import static nl.rivm.screenit.util.StringUtil.propertyChain;
import static nl.topicuszorg.organisatie.model.Adres_.PLAATS;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_COLOSCOPIECENTRUM_ORG_BEHEER, Recht.GEBRUIKER_MAMMA_MAMMAPOLI_ORG_BEHEER, Recht.GEBRUIKER_MAMMA_RADIOLOGIEAFDELING_ORG_BEHEER,
		Recht.GEBRUIKER_BEHEER_CC_LOCATIES, Recht.GEBRUIKER_BEHEER_CC_GEBIEDEN, Recht.GEBRUIKER_INPAKCENTRUM_ORG_BEHEER, Recht.GEBRUIKER_LABORATORIA_BEHEER,
		Recht.GEBRUIKER_PA_LABORATORIA_BEHEER, Recht.GEBRUIKER_RIVM_BEHEER, Recht.GEBRUIKER_SCREENINGS_ORG_BEHEER,
		Recht.GEBRUIKER_ZORGINSTELLING_ORG_BEHEER, Recht.GEBRUIKER_COLOSCOPIELOCATIE_ORG_BEHEER, Recht.GEBRUIKER_HUISARTSENPRAKTIJKEN_BEHEER,
		Recht.GEBRUIKER_BMHK_LABORATORIA_BEHEER, Recht.GEBRUIKER_CENTRALE_EENHEID_ORG_BEHEER, Recht.GEBRUIKER_BEOORDELINGSEENHEID_ORG_BEHEER },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
	level = ToegangLevel.INSTELLING)
@ZoekenContextMenuItem
public class OrganisatieZoeken extends OrganisatieBeheer
{
	private final IModel<List<OrganisatieType>> selectedOrganisatieTypes;

	@SpringBean
	private AutorisatieService autorisatieService;

	private final Form<Instelling> zoekForm;

	public OrganisatieZoeken()
	{
		setCurrentSelectedOrganisatie(null);
		setCurrentSelectedMedewerker(null);

		final var refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		IModel<Instelling> criteriaModel;
		if (ScreenitSession.get().isZoekObjectGezetForComponent(OrganisatieZoeken.class))
		{
			criteriaModel = (IModel<Instelling>) ScreenitSession.get().getZoekObject(OrganisatieZoeken.class);
		}
		else
		{
			var zoekObject = new Instelling();
			zoekObject.add(new Adres());
			zoekObject.setOrganisatieMedewerkers(new ArrayList<>());
			zoekObject.getOrganisatieMedewerkers().add(new InstellingGebruiker());
			zoekObject.getOrganisatieMedewerkers().get(0).setMedewerker(new Gebruiker());
			zoekObject.setActief(true);
			criteriaModel = Model.of(zoekObject);
		}
		var choices = autorisatieService.getOrganisatieTypes(ScreenitSession.get().getLoggedInInstellingGebruiker(), true);
		if (ScreenitSession.get().isZoekObjectGezetForComponent("OrganisatieZoeken.selectedOrganisatieTypes"))
		{
			selectedOrganisatieTypes = (IModel<List<OrganisatieType>>) ScreenitSession.get().getZoekObject("OrganisatieZoeken.selectedOrganisatieTypes");
		}
		else
		{
			selectedOrganisatieTypes = new ListModel<>(new ArrayList<>());
		}

		var columns = new ArrayList<IColumn<Instelling, String>>();
		columns.add(new PropertyColumn<>(Model.of("Naam organisatie"), NAAM, NAAM));
		columns.add(new PropertyColumn<>(Model.of("Adres"), propertyChain(ADRESSEN, Adres_.STRAAT), "adressen[0].adres")
		{

			@Override
			public IModel<?> getDataModel(IModel<Instelling> rowModel)
			{
				if (rowModel.getObject().getOrganisatieType() == OrganisatieType.HUISARTS)
				{
					return new PropertyModel<>(rowModel, "postadres.adres");
				}
				else
				{
					return super.getDataModel(rowModel);
				}
			}

		});
		columns.add(new PropertyColumn<>(Model.of("Plaats"), propertyChain(ADRESSEN, PLAATS), "adressen[0].plaats")
		{

			@Override
			public IModel<?> getDataModel(IModel<Instelling> rowModel)
			{
				if (rowModel.getObject().getOrganisatieType() == OrganisatieType.HUISARTS)
				{
					return new PropertyModel<>(rowModel, "postadres.woonplaats.naam");
				}
				else
				{
					return super.getDataModel(rowModel);
				}
			}

		});
		columns.add(new PropertyColumn<>(Model.of("Soort organisatie"), ORGANISATIE_TYPE, ORGANISATIE_TYPE));

		columns.add(new ActiefPropertyColumn<>(Model.of(""), ACTIEF, refreshContainer, criteriaModel.getObject()));

		var organisaties = new ScreenitDataTable<>("organisaties", columns,
			new OrganisatieDataProvider(criteriaModel, selectedOrganisatieTypes, null, "naam"), 10, new Model<>("organisaties"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<Instelling> model)
			{
				var organisatie = model.getObject();
				organisatie.getOrganisatieMedewerkers().size();
				setCurrentSelectedOrganisatie(organisatie);

				BasePage.markeerFormulierenOpgeslagen(target);

				for (var menuItem : OrganisatieBeheer.createContextMenu())
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
		zoekForm = new Form<>("zoekForm", (IModel<Instelling>) getDefaultModel());
		add(zoekForm);

		zoekForm.add(new TextField<>("naam"));
		zoekForm.add(new TextField<>("organisatieMedewerkers[0].medewerker.achternaam"));
		zoekForm.add(new TextField<>("adressen[0].plaats"));
		zoekForm.add(new PostcodeField("adressen[0].postcode").setAlleenCijfersToegestaan(true));
		zoekForm.add(new TextField<>("organisatieMedewerkers[0].medewerker.uzinummer"));
		zoekForm.add(new TextField<>("uziAbonneenummer"));
		zoekForm.add(new TextField<>("email"));

		zoekForm.add(new ListMultipleChoice<>("selectedOrganisatieTypes", new PropertyModel<List<OrganisatieType>>(this, "selectedOrganisatieTypes"), choices,
			new EnumChoiceRenderer<>()));

		zoekForm.add(new AjaxSubmitLink("zoeken", zoekForm)
		{

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
		var list = autorisatieService.getOrganisatieTypes(ScreenitSession.get().getLoggedInInstellingGebruiker(), Actie.TOEVOEGEN, true);
		list.remove(OrganisatieType.RIVM);
		list.remove(OrganisatieType.HUISARTS);
		var nieuwOrganisatieTypes = new ListView<>("nieuwOrganisatieTypes", list)
		{

			@Override
			protected void populateItem(final ListItem<OrganisatieType> item)
			{
				var nieuwOrganisatie = new AjaxLink<Void>("nieuwOrganisatie")
				{
					@Override
					public void onClick(AjaxRequestTarget target)
					{
						var nieuweOrganisatie = OrganisatieUtil.maakOrganisatie(item.getModelObject(), ScreenitSession.get().getLoggedInInstellingGebruiker().getOrganisatie());
						setResponsePage(new OrganisatieBasisgegevens(ModelUtil.ccModel(nieuweOrganisatie)));
					}

				};
				item.add(nieuwOrganisatie);

				nieuwOrganisatie.add(new EnumLabel<>("label", item.getModelObject()));
			}

		};
		add(nieuwOrganisatieTypes);
		var toevoegen = new Button("toevoegen");
		toevoegen.setVisible(!list.isEmpty());
		add(toevoegen);
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
