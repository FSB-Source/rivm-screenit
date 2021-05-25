package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.adhoc;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaAdhocMeekijkverzoekWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ExportToXslLink;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaScreeningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaBeTabelCounterPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
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
	recht = { Recht.GEBRUIKER_AD_HOC_MEEMKIJKVERZOEK_WERKLIJST },
	organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaAdhocMeekijkverzoekOnderzoekenWerklijstPage extends MammaScreeningBasePage
{

	@SpringBean
	private MammaBeoordelingService beoordelingService;

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	private Form<MammaAdhocMeekijkverzoekWerklijstZoekObject> zoekForm;

	private IModel<MammaAdhocMeekijkverzoekWerklijstZoekObject> zoekObjectModel = new CompoundPropertyModel<>(new MammaAdhocMeekijkverzoekWerklijstZoekObject());

	private IModel<List<MammaScreeningsEenheid>> screeningsEenhedenModel = new SimpleListHibernateModel<>(new ArrayList<>());

	private ScreenitListMultipleChoice<MammaScreeningsEenheid> screeningsEenhedenSelector;

	private WebMarkupContainer refreshContainer;

	public MammaAdhocMeekijkverzoekOnderzoekenWerklijstPage()
	{
		wijzigIDS7Role(MammobridgeRole.RADIOLOGIST_ANONIEM);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		MammaAdhocMeekijkverzoekOnderzoekenDataProvider onderzoekDataProvider = new MammaAdhocMeekijkverzoekOnderzoekenDataProvider(zoekObjectModel);
		zoekObjectModel.getObject().setStatus(MammaVisitatieOnderzoekStatus.NIET_GEZIEN);
		List<MammaScreeningsEenheid> mogelijkeScreeningsEenheden = screeningsEenheidService.getActieveScreeningsEenheden();
		zoekObjectModel.getObject().setScreeningsEenheden(mogelijkeScreeningsEenheden);
		screeningsEenhedenModel.setObject(mogelijkeScreeningsEenheden);
		zoekForm = new Form<>("form", zoekObjectModel);

		screeningsEenhedenSelector = createScreeningsEenhedenSelector();
		zoekForm.add(screeningsEenhedenSelector);

		ScreenitDropdown<MammaVisitatieOnderzoekStatus> onderzoekStatusSelector = new ScreenitDropdown<>(
			"status",
			Arrays.asList(MammaVisitatieOnderzoekStatus.values()),
			new EnumChoiceRenderer<>(this));
		onderzoekStatusSelector.setNullValid(true);
		zoekForm.add(onderzoekStatusSelector);

		IndicatingAjaxSubmitLink zoekenButton = new IndicatingAjaxSubmitLink("zoeken", zoekForm)
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				target.add(refreshContainer, zoekForm);
			}
		};
		zoekForm.setDefaultButton(zoekenButton);
		zoekForm.add(zoekenButton);
		add(zoekForm);

		refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		List<IColumn<MammaAdhocMeekijkverzoek, String>> columns = new ArrayList<>();
		columns.add(new DateTimePropertyColumn<>(Model.of("Onderzoeksdatum"), "onderzoek.creatieDatum", "onderzoek.creatieDatum",
			Constants.getDateTimeSecondsFormat()));
		columns.add(new PropertyColumn<>(Model.of("Volgnummer"), "volgnummer"));
		if (ScreenitSession.get().getInstelling().getOrganisatieType() != OrganisatieType.KWALITEITSPLATFORM)
		{
			columns.add(new ClientColumn<>("persoon.achternaam", "onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client"));
			columns.add(new GeboortedatumColumn<>("persoon.geboortedatum", "onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon"));
			columns.add(new PropertyColumn<>(Model.of("BSN"), "persoon.bsn", "onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon.bsn"));
		}
		columns.add(new PropertyColumn<>(Model.of("Reden"), "reden"));
		columns.add(new PropertyColumn<>(Model.of("SE"), "se.naam", "onderzoek.screeningsEenheid.naam"));
		columns.add(new EnumPropertyColumn<>(Model.of("Status"), "status", "status", this));

		ScreenitDataTable<MammaAdhocMeekijkverzoek, String> table = new ScreenitDataTable<MammaAdhocMeekijkverzoek, String>("resultaten", columns,
			onderzoekDataProvider, 10,
			Model.of("onderzoek(en)"))
		{

			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaAdhocMeekijkverzoek> model)
			{
				if (ScreenitSession.get().getInstelling().getOrganisatieType() == OrganisatieType.KWALITEITSPLATFORM)
				{
					SortParam<String> sortParam = onderzoekDataProvider.getSort();
					openBesprekenScherm(target, model, new SortState<String>(sortParam.getProperty(), sortParam.isAscending()));
				}
			}

			@Override
			public Panel getCustomPanel(String id)
			{
				IModel<Integer> gezienModel = new IModel<Integer>()
				{
					@Override
					public Integer getObject()
					{
						return kwaliteitscontroleService.getAantalGezienAdhocMeekijkverzoekOnderzoeken(zoekObjectModel.getObject());
					}

				};

				IModel<Integer> nietGezienModel = new IModel<Integer>()
				{
					@Override
					public Integer getObject()
					{
						return (int) getItemCount() - gezienModel.getObject();
					}
				};

				return new MammaBeTabelCounterPanel(id, nietGezienModel, gezienModel);
			}
		};
		refreshContainer.add(table);
		refreshContainer.add(new ExportToXslLink<>("csv", "Onderzoek(en)", table));
	}

	private void openBesprekenScherm(AjaxRequestTarget target, IModel<MammaAdhocMeekijkverzoek> model,
		SortState<String> sortState)
	{
		Map<Long, Long> onderzoekenIdMapping = new LinkedHashMap<>();
		for (MammaAdhocMeekijkverzoek meekijkverzoek : kwaliteitscontroleService.zoekAdhocMeekijkverzoekOnderzoeken(zoekObjectModel.getObject(), -1, -1, sortState))
		{
			onderzoekenIdMapping.put(meekijkverzoek.getOnderzoek().getId(), meekijkverzoek.getId());
		}

		MammaAdhocMeekijkverzoek verzoek = model.getObject();
		setResponsePage(new MammaAdhocMeekijkverzoekOnderzoekInzienPage(verzoek.getOnderzoek().getId(), onderzoekenIdMapping, getClass()));
	}

	private ScreenitListMultipleChoice<MammaScreeningsEenheid> createScreeningsEenhedenSelector()
	{
		return new ScreenitListMultipleChoice<>(
			"screeningsEenheden",
			new PropertyModel<List<MammaScreeningsEenheid>>(zoekObjectModel, "screeningsEenheden"), screeningsEenhedenModel,
			new ChoiceRenderer<>("naam"));
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.adhockwaliteitscontrole.onderzoeken",
			MammaAdhocMeekijkverzoekOnderzoekenWerklijstPage.class));

		return contextMenuItems;
	}

}
