package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie;

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
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.dto.cervix.GekoppeldeUitstrijkendArtsZoekObject;
import nl.rivm.screenit.main.service.cervix.impl.GekoppeldeUitstrijkendArtsenDataProviderServiceImpl;
import nl.rivm.screenit.main.util.WicketSpringDataUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ExportToXslLink;
import nl.rivm.screenit.main.web.component.table.NavigeerNaarCellPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.huisarts.AanvullendeHaGegevensPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsLocatieMutatieSoort;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.jetbrains.annotations.NotNull;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = { Recht.GEBRUIKER_SCREENINGS_ORG_BEHEER,
	Recht.GEBRUIKER_BMHK_LABORATORIA_BEHEER }, bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX })
public class GekoppeldeUitstrijkendArtsenPage extends OrganisatieBeheer
{

	private IModel<List<Gemeente>> gemeentesUitOrganisatie = new SimpleListHibernateModel<>(new ArrayList<>());

	@SpringBean
	private GekoppeldeUitstrijkendArtsenDataProviderServiceImpl gekoppeldeUitstrijkendArtsenDataProviderService;

	@SpringBean
	private AutorisatieService autorisatieService;

	private IModel<List<Gemeente>> zoekGemeente = ModelUtil.listRModel(new ArrayList<>());

	private Date zoekMutatiedatumVanaf = null;

	private Date zoekMutatiedatumTot = null;

	private String zoekAgbCode = "";

	private List<CervixHuisartsLocatieMutatieSoort> zoekMutatieSoort = Arrays.asList(CervixHuisartsLocatieMutatieSoort.values());

	public GekoppeldeUitstrijkendArtsenPage()
	{
		if (getCurrentSelectedOrganisatie() instanceof BMHKLaboratorium)
		{
			gemeentesUitOrganisatie = new SimpleListHibernateModel<>(((BMHKLaboratorium) getCurrentSelectedOrganisatie()).getGemeentes());
		}
		else if (getCurrentSelectedOrganisatie() instanceof ScreeningOrganisatie)
		{
			gemeentesUitOrganisatie = new SimpleListHibernateModel<>(((ScreeningOrganisatie) getCurrentSelectedOrganisatie()).getGemeentes());
		}
		final var dialog = new BootstrapDialog("dialog");
		add(dialog);
		final var medewerkerContainer = new WebMarkupContainer("medewerkerContainer");
		medewerkerContainer.setOutputMarkupId(true);

		add(new OrganisatiePaspoortPanel("paspoort", ModelUtil.cRModel(getCurrentSelectedOrganisatie())));

		Form<?> zoekForm = new Form<>("zoekForm");
		add(zoekForm);

		zoekForm.add(new TextField<>("zoekAgbCode", new PropertyModel<>(this, "zoekAgbCode")));
		zoekForm.add(
			new ScreenitListMultipleChoice<>("zoekGemeente", new PropertyModel<>(this, "zoekGemeente"), gemeentesUitOrganisatie,
				new ChoiceRenderer<>("naam", "code")
				{
					@Override
					public Object getDisplayValue(Gemeente object)
					{
						var gemeente = super.getDisplayValue(object);
						if (object.getCode() != null)
						{
							return gemeente + " (" + object.getCode() + ")";
						}
						else
						{
							return gemeente;
						}
					}
				}));

		ComponentHelper.addTextField(zoekForm, "zoekMutatiedatumVanaf", false, 10, Date.class, false).setModel(new PropertyModel<>(this, "zoekMutatiedatumVanaf"));
		ComponentHelper.addTextField(zoekForm, "zoekMutatiedatumTot", false, 10, Date.class, false).setModel(new PropertyModel<>(this, "zoekMutatiedatumTot"));
		var mutatieSoorten = Arrays.asList(CervixHuisartsLocatieMutatieSoort.values());
		var multiSoortSelect = new ScreenitListMultipleChoice<>("mutatiesoort",
			new PropertyModel<>(this, "zoekMutatieSoort"),
			mutatieSoorten, new EnumChoiceRenderer<>());
		multiSoortSelect.setRequired(true);
		zoekForm.add(multiSoortSelect);
		zoekForm.add(new AjaxSubmitLink("zoeken", zoekForm)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				target.add(medewerkerContainer);
			}
		});

		var columns = new ArrayList<IColumn<CervixHuisartsLocatie, String>>();
		columns.add(new PropertyColumn<>(Model.of("Naam"), "huisarts.naam"));
		columns.add(new PropertyColumn<>(Model.of("Naam locatie"), "naam"));
		columns.add(new PropertyColumn<>(Model.of("AGB-code"), "huisarts.agbcode"));
		columns.add(new PropertyColumn<>(Model.of("Locatie id"), "id"));
		columns.add(new PropertyColumn<>(Model.of("Postadres"), "locatieAdres"));
		columns.add(new PropertyColumn<>(Model.of("Plaats"), "locatieAdres.woonplaats.naam"));
		columns.add(new PropertyColumn<>(Model.of("Gemeente"), "locatieAdres.woonplaats.gemeente.naam"));
		columns.add(new PropertyColumn<>(Model.of("Mutatiedatum"), "mutatiedatum", "mutatiedatum")
		{
			@Override
			public void populateItem(Item<ICellPopulator<CervixHuisartsLocatie>> item, String componentId, IModel<CervixHuisartsLocatie> rowModel)
			{

				final var format = new SimpleDateFormat("dd-MM-yyyy HH:mm");
				var mutatiedatum = rowModel.getObject().getMutatiedatum();
				var tekst = "Mutatiedatum niet beschikbaar";
				if (mutatiedatum != null)
				{
					tekst = format.format(mutatiedatum);
				}
				item.add(new Label(componentId, new Model<>(tekst)));
			}
		});
		columns.add(new EnumPropertyColumn<>(Model.of("Mutatiesoort"), "mutatieSoort", "mutatieSoort"));
		if (ScreenitSession.get().checkPermission(OrganisatieType.HUISARTS.getRecht(), Actie.INZIEN))
		{
			columns.add(new PropertyColumn<>(Model.of(""), null)
			{
				@Override
				public void populateItem(Item<ICellPopulator<CervixHuisartsLocatie>> item, String componentId, IModel<CervixHuisartsLocatie> rowModel)
				{
					var cervixHuisartsIModel = ModelUtil.cModel(rowModel.getObject().getHuisarts());
					item.add(new NavigeerNaarCellPanel<>(componentId, cervixHuisartsIModel)
					{
						@Override
						protected boolean magNavigerenNaar(IModel<CervixHuisarts> rowModel)
						{
							return GekoppeldeUitstrijkendArtsenPage.this.magNavigerenNaar(rowModel);
						}

						@Override
						protected void onNavigeerNaar(AjaxRequestTarget target, IModel<CervixHuisarts> rowModel)
						{
							GekoppeldeUitstrijkendArtsenPage.this.onNavigeerNaar(rowModel, target);
						}
					});
				}
			});
		}

		IModel<String> totaalLabel = new Model<>("BMHK arts(en)");

		var dataTable = new ScreenitDataTable<>("organisaties", columns,
			new SortableDataProvider<>()
			{
				private GekoppeldeUitstrijkendArtsZoekObject maakZoekObject()
				{
					var zoekObject = new GekoppeldeUitstrijkendArtsZoekObject();
					zoekObject.setGemeentes(getGemeentes());
					zoekObject.setAgbCode(zoekAgbCode);
					zoekObject.setMutatieSoorten(zoekMutatieSoort);
					zoekObject.setMutatieDatumVanaf(DateUtil.toLocalDate(zoekMutatiedatumVanaf));
					zoekObject.setMutatieDatumTotEnMet(DateUtil.toLocalDate(zoekMutatiedatumTot));
					return zoekObject;
				}

				@Override
				public Iterator<? extends CervixHuisartsLocatie> iterator(long first, long count)
				{
					if (getSort() == null)
					{
						setSort("mutatiedatum", SortOrder.ASCENDING);
					}
					var sort = WicketSpringDataUtil.toSpringSort(getSort());
					return gekoppeldeUitstrijkendArtsenDataProviderService.findPage(first, count, maakZoekObject(), sort).iterator();
				}

				@Override
				public long size()
				{
					return gekoppeldeUitstrijkendArtsenDataProviderService.size(maakZoekObject());
				}

				@NotNull
				private List<Gemeente> getGemeentes()
				{
					List<Gemeente> gemeentes = new ArrayList<>();
					if (getZoekGemeente().isEmpty())
					{
						gemeentes.addAll(gemeentesUitOrganisatie.getObject());
					}
					else
					{
						gemeentes.addAll(getZoekGemeente());
					}
					return gemeentes;
				}

				@Override
				public IModel<CervixHuisartsLocatie> model(CervixHuisartsLocatie object)
				{
					return ModelUtil.sModel(object);
				}

			}, totaalLabel);

		medewerkerContainer.add(dataTable);
		medewerkerContainer.add(new ExportToXslLink<>("csv", "BMHK artsen", dataTable));

		add(medewerkerContainer);
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return false;
	}

	public List<Gemeente> getZoekGemeente()
	{
		return zoekGemeente.getObject();
	}

	@SuppressWarnings("Onderstaande methode is nodig voor reflectie, zie: new PropertyModel<>(this, \"zoekGemeente\")")
	public void setZoekGemeente(List<Gemeente> zoekGemeente)
	{
		this.zoekGemeente = ModelUtil.listRModel(zoekGemeente);
	}

	protected void onNavigeerNaar(IModel<CervixHuisarts> rowModel, AjaxRequestTarget target)
	{
		var cervixHuisarts = rowModel.getObject();
		setCurrentSelectedOrganisatie(cervixHuisarts);
		setCurrentSelectedMedewerker(null);
		setResponsePage(new AanvullendeHaGegevensPage());
	}

	protected boolean magNavigerenNaar(IModel<CervixHuisarts> rowModel)
	{
		var cervixHuisarts = rowModel.getObject();
		var loggedInInstellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		return autorisatieService.getActieVoorOrganisatie(loggedInInstellingGebruiker, cervixHuisarts, Recht.GEBRUIKER_SCREENINGS_ORG_BEHEER,
			Recht.GEBRUIKER_BMHK_LABORATORIA_BEHEER) != null;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(zoekGemeente);
		ModelUtil.nullSafeDetach(gemeentesUitOrganisatie);
	}
}
