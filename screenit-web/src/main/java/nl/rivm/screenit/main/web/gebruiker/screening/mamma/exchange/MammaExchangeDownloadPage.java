package nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange;

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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.mamma.MammaUitwisselportaalService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.PollingAbstractAjaxTimerBehavior;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.component.table.UploadDocumentDownloadColumn;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;
import nl.topicuszorg.wicket.search.column.HibernateCheckBoxListContainer;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.time.Duration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	recht = { Recht.GEBRUIKER_MAMMA_EXCHANGE },
	organisatieTypeScopes = { OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RADIOLOGIEAFDELING, OrganisatieType.MAMMAPOLI, OrganisatieType.ZORGINSTELLING,
		OrganisatieType.RIVM },
	checkScope = true)
public class MammaExchangeDownloadPage extends MammaExchangeBasePage
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaExchangeDownloadPage.class);

	@SpringBean
	private MammaUitwisselportaalService uitwisselPortaalService;

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	private WebMarkupContainer passport = null;

	private WebMarkupContainer contentContainer = null;

	private HibernateCheckBoxListContainer<MammaOnderzoek> selectedOnderzoeken = new HibernateCheckBoxListContainer<>();

	private Form<Void> passportForm;

	private ScreenitDataTable<MammaDownloadOnderzoekenVerzoek, String> werklijst;

	private PollingAbstractAjaxTimerBehavior timer;

	private boolean firstTimeKlaar = false;

	private IModel<MammaDownloadOnderzoekenVerzoek> verzoekFilter;

	public MammaExchangeDownloadPage()
	{
		super();
		passportForm = new Form<>("form");
		add(passportForm);
		createEmptyPassportContainer();
		createEmptyContentContainer();
		createStartDownloaden();
		createWerklijst();
	}

	private void createWerklijst()
	{
		List<IColumn<MammaDownloadOnderzoekenVerzoek, String>> columns = new ArrayList<>();

		columns.add(new DateTimePropertyColumn<>(Model.of("Aangemaakt"), "aangemaaktOp", "aangemaaktOp", Constants.getDateTimeFormat()));
		columns.add(new PropertyColumn<MammaDownloadOnderzoekenVerzoek, String>(Model.of("Door"), "aangemaaktDoor.medewerker")
		{
			@Override
			public IModel<?> getDataModel(IModel<MammaDownloadOnderzoekenVerzoek> rowModel)
			{
				IModel<?> dataModel = super.getDataModel(rowModel);
				Gebruiker medewerker = (Gebruiker) dataModel.getObject();
				return new Model(NaamUtil.getNaamGebruiker(medewerker));
			}
		});
		columns.add(new DateTimePropertyColumn<>(Model.of("Gewijzigd"), "gewijzigdOp", "gewijzigdOp", Constants.getDateTimeFormat()));
		columns.add(new EnumPropertyColumn<>(Model.of("Status"), "status", "status", this));
		columns.add(new ClientColumn<>("onderzoeken[0].onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client"));
		columns.add(new PropertyColumn<>(Model.of("Bsn"), "onderzoeken[0].onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon.bsn"));
		columns.add(new GeboortedatumColumn<>("onderzoeken[0].onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon"));
		columns.add(new AbstractColumn<MammaDownloadOnderzoekenVerzoek, String>(Model.of("Melding"))
		{

			@Override
			public void populateItem(Item<ICellPopulator<MammaDownloadOnderzoekenVerzoek>> cellItem, String componentId, IModel<MammaDownloadOnderzoekenVerzoek> rowModel)
			{
				BestandStatus status = rowModel.getObject().getStatus();
				if (status == BestandStatus.VERWERKT || status == BestandStatus.CRASH)
				{
					cellItem.add(new Label(componentId, maakMeldingLabel(rowModel)));
				}
				else
				{
					cellItem.add(new EmptyPanel(componentId));
				}
			}

			private String maakMeldingLabel(IModel<MammaDownloadOnderzoekenVerzoek> rowModel)
			{
				String label = "";

				for (MammaDownloadOnderzoek onderzoek : rowModel.getObject().getOnderzoeken())
				{
					label += "Onderzoeksdatum " + Constants.getDateTimeFormat().format(onderzoek.getOnderzoek().getCreatieDatum()) + ": "
						+ onderzoek.getStatusMelding();
					if (!StringUtils.trim(label).endsWith("."))
					{
						label += ". ";
					}
					if (!label.endsWith(" "))
					{
						label += " ";
					}
				}
				return label;
			}

		});
		columns.add(new UploadDocumentDownloadColumn<MammaDownloadOnderzoekenVerzoek, String>(Model.of("Download Zip"), "zipBestand")
		{

			@Override
			public void populateItem(Item<ICellPopulator<MammaDownloadOnderzoekenVerzoek>> cellItem, String componentId, IModel<MammaDownloadOnderzoekenVerzoek> rowModel)
			{
				BestandStatus status = rowModel.getObject().getStatus();
				if ((status == BestandStatus.VERWERKT || status == BestandStatus.CRASH) && !heeftClientBezwaarTegenUitwisseling(rowModel.getObject()))
				{
					super.populateItem(cellItem, componentId, rowModel);
				}
				else
				{
					cellItem.add(new EmptyPanel(componentId));
				}
			}

			@Override
			protected void onBeforeDownloadClick(AjaxRequestTarget target, IModel<MammaDownloadOnderzoekenVerzoek> rowModel)
			{
				uitwisselPortaalService.updateDownloadVerzoekInformatie(rowModel.getObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				target.add(werklijst);
			}
		});
		columns.add(new DateTimePropertyColumn<>(Model.of("Gedownload op"), "gedownloadOp", "gedownloadOp", Constants.getDateTimeFormat()));
		columns.add(new AbstractColumn<MammaDownloadOnderzoekenVerzoek, String>(Model.of("Opnieuw ophalen"))
		{

			@Override
			public void populateItem(Item<ICellPopulator<MammaDownloadOnderzoekenVerzoek>> cellItem, String componentId, IModel<MammaDownloadOnderzoekenVerzoek> rowModel)
			{
				BestandStatus status = rowModel.getObject().getStatus();
				if ((status == BestandStatus.VERWERKT || status == BestandStatus.CRASH) && !heeftClientBezwaarTegenUitwisseling(rowModel.getObject()))
				{
					cellItem.add(new AjaxImageCellPanel<MammaDownloadOnderzoekenVerzoek>(componentId, rowModel, "icon-refresh")
					{
						@Override
						protected void onClick(AjaxRequestTarget target)
						{
							try
							{
								uitwisselPortaalService.resetDownloadVerzoek(getModelObject());
								uitwisselPortaalService.startDownloading();
								info("Verzoek voor ophalen van beelden en verslag is opnieuw gestart.");
								timer.restartTimer(target);
								target.add(werklijst);
							}
							catch (IOException e)
							{
								LOG.error("Fout bij starten download verzoek", e);
								error("Er is iets fout gegaan bij het opnieuw starten van het download verzoek, vraag Uw contact persoon voor hulp.");
							}
						}
					});
				}
				else
				{
					cellItem.add(new EmptyPanel(componentId));
				}
			}

		});
		verzoekFilter = ModelUtil.cModel(new MammaDownloadOnderzoekenVerzoek());

		if (ScreenitSession.get().getInstelling().getOrganisatieType() != OrganisatieType.RIVM)
		{
			verzoekFilter.getObject().setAangemaaktDoor(ScreenitSession.get().getLoggedInInstellingGebruiker());
		}
		MammaDownloadOnderzoekenVerzoekenProvider dataProvider = new MammaDownloadOnderzoekenVerzoekenProvider(verzoekFilter);
		werklijst = new ScreenitDataTable<MammaDownloadOnderzoekenVerzoek, String>("werklijst", columns, dataProvider,
			Model.of("verzoek(en)"))
		{
			@Override
			protected boolean isRowClickable(IModel<MammaDownloadOnderzoekenVerzoek> rowModel)
			{
				return false;
			}
		};
		werklijst.setOutputMarkupId(true);
		add(werklijst);

		createTimer(dataProvider);
	}

	private boolean heeftClientBezwaarTegenUitwisseling(MammaDownloadOnderzoekenVerzoek downloadOnderzoekenVerzoek)
	{
		if (downloadOnderzoekenVerzoek.getOnderzoeken() != null && downloadOnderzoekenVerzoek.getOnderzoeken().size() != 0)
		{
			Client client = baseBeoordelingService.getClientVanBeoordeling(downloadOnderzoekenVerzoek.getOnderzoeken().get(0).getOnderzoek().getLaatsteBeoordeling());
			return BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS, Bevolkingsonderzoek.MAMMA);
		}
		return false;
	}

	private void createTimer(MammaDownloadOnderzoekenVerzoekenProvider dataProvider)
	{
		timer = new PollingAbstractAjaxTimerBehavior(Duration.seconds(3))
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onTimer(AjaxRequestTarget target)
			{
				super.onTimer(target);
				target.add(werklijst);
				startStopTimerIfNeeded(target);
			}

			private void startStopTimerIfNeeded(AjaxRequestTarget target)
			{
				verzoekFilter.getObject().setStatus(BestandStatus.VERWERKT);
				if (dataProvider.size() == 0)
				{
					if (firstTimeKlaar)
					{
						timer.stop(target);
					}
					firstTimeKlaar = !firstTimeKlaar;
				}
				else
				{
					timer.restartTimer(target);
					firstTimeKlaar = false;
				}
				verzoekFilter.getObject().setStatus(null);

			}
		};
		add(timer);
	}

	private void createStartDownloaden()
	{
		contentContainer.add(new IndicatingAjaxSubmitLink("startDownloaden")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				List<MammaOnderzoek> onderzoeken = selectedOnderzoeken.getList();
				if (onderzoeken.isEmpty())
				{
					error("Er zijn geen onderzoeken geselecteerd.");
				}
				else
				{
					try
					{
						uitwisselPortaalService.maakDownloadVerzoek(onderzoeken, ScreenitSession.get().getLoggedInInstellingGebruiker());
						uitwisselPortaalService.startDownloading();
						info("Verzoek voor ophalen van beelden en verslag van geselecteerde onderzoeken is gestart");
						target.add(werklijst);
						timer.restartTimer(target);
						selectedOnderzoeken.clear();
						clientOpt = null;
						updateContent();
					}
					catch (IOException e)
					{
						LOG.error("Fout bij starten download verzoek", e);
						error("Er is iets fout gegaan bij het starten van het download verzoek, vraag Uw contact persoon voor hulp.");
					}
				}
			}
		});
		add(new IndicatingAjaxLink<Void>("refresh")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				target.add(werklijst);
			}

		});
	}

	@Override
	protected void updateContent()
	{
		AjaxRequestTarget target = RequestCycle.get().find(AjaxRequestTarget.class).orElse(null);
		if (clientOpt != null)
		{
			passport = new ClientPaspoortPanel("paspoort", clientOpt);
			passport.setOutputMarkupId(true);
			passport.setOutputMarkupPlaceholderTag(true);
			passport.setVisible(true);
			passportForm.addOrReplace(passport);
			target.add(passport);

			createContentContainer(clientOpt, target);
		}
		else
		{
			passport.setVisible(false);
			contentContainer.setVisible(false);
			target.add(passport, contentContainer);
		}
	}

	private void createContentContainer(IModel<Client> clientOpt, AjaxRequestTarget target)
	{
		RepeatingView repeatingView = new RepeatingView("contentRepeater");
		repeatingView.setOutputMarkupId(true);
		repeatingView.setVisible(true);
		new ArrayList<>(clientOpt.getObject().getMammaDossier().getScreeningRondes()) 
			.stream()
			.filter(ronde -> ronde.getLaatsteUitnodiging() != null && ronde.getLaatsteUitnodiging().getLaatsteAfspraak() != null &&
				ronde.getLaatsteUitnodiging().getLaatsteAfspraak().getOnderzoek() != null &&
				ronde.getLaatsteUitnodiging().getLaatsteAfspraak().getOnderzoek().getLaatsteBeoordeling() != null &&
				MammaMammografieIlmStatus.beeldenBeschikbaar(ronde.getLaatsteUitnodiging().getLaatsteAfspraak().getOnderzoek().getMammografie().getIlmStatus()) &&
				Arrays.asList(MammaBeoordelingStatus.UITSLAG_ONGUNSTIG, MammaBeoordelingStatus.UITSLAG_GUNSTIG)
					.contains(ronde.getLaatsteUitnodiging().getLaatsteAfspraak().getOnderzoek().getLaatsteBeoordeling().getStatus()))
			.sorted(Comparator.comparing(MammaScreeningRonde::getCreatieDatum).reversed())
			.map(ModelUtil::sModel)
			.forEachOrdered(ronde ->
			{
				Component rondeDownloadPanel = new MammaExchangeDownloadRondePanel(repeatingView.newChildId(), selectedOnderzoeken, ronde);
				rondeDownloadPanel.setOutputMarkupId(true);
				repeatingView.add(rondeDownloadPanel);
			});
		contentContainer.setVisible(true);
		contentContainer.addOrReplace(repeatingView);
		target.add(contentContainer);
	}

	private void createEmptyContentContainer()
	{
		contentContainer = new WebMarkupContainer("content");
		contentContainer.setOutputMarkupId(true);
		contentContainer.setVisible(false);
		contentContainer.setOutputMarkupPlaceholderTag(true);
		passportForm.add(contentContainer);
		contentContainer.add(new EmptyPanel("contentRepeater").setVisible(false));
	}

	private void createEmptyPassportContainer()
	{
		passport = new EmptyPanel("paspoort");
		passport.setOutputMarkupId(true);
		passport.setVisible(false);
		passport.setOutputMarkupPlaceholderTag(true);
		passportForm.add(passport);
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(verzoekFilter);
	}
}
