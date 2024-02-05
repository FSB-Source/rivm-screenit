package nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange.download;

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

import java.io.IOException;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.mamma.MammaUitwisselportaalService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.PollingAbstractAjaxTimerBehavior;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.IndicatingAjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange.MammaExchangeBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
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
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.request.resource.IResource;
import org.apache.wicket.request.resource.ResourceReference;
import org.apache.wicket.spring.injection.annot.SpringBean;
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

	private WebMarkupContainer passport = null;

	private WebMarkupContainer contentContainer = null;

	private final HibernateCheckBoxListContainer<MammaOnderzoek> selectedOnderzoeken = new HibernateCheckBoxListContainer<>();

	private final Form<Void> passportForm;

	private ScreenitDataTable<MammaDownloadOnderzoekenVerzoek, String> werklijst;

	private PollingAbstractAjaxTimerBehavior timer;

	private boolean firstTimeKlaar = false;

	private IModel<MammaDownloadOnderzoekenVerzoek> verzoekFilter;

	private ResourceReference downloadResourceReference;

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(JavaScriptHeaderItem.forUrl("assets/js/checkbox/checkboxOnderzoeken.js"));
	}

	public MammaExchangeDownloadPage()
	{
		super();
		initDownloadResourceReference();
		passportForm = new Form<>("form");
		add(passportForm);
		createEmptyPassportContainer();
		createEmptyContentContainer();
		createStartDownloaden();
		createWerklijst();
	}

	private void initDownloadResourceReference()
	{
		var downloadResource = new MammaExchangeDownloadResource();
		downloadResourceReference = new ResourceReference("mammaExchangeDownloadResourceReference")
		{
			@Override
			public IResource getResource()
			{
				return downloadResource;
			}
		};
	}

	private void createWerklijst()
	{
		List<IColumn<MammaDownloadOnderzoekenVerzoek, String>> columns = new ArrayList<>();

		columns.add(new DateTimePropertyColumn<>(Model.of("Aangemaakt"), "aangemaaktOp", "aangemaaktOp", Constants.getDateTimeFormat()));
		columns.add(new PropertyColumn<>(Model.of("Door"), "aangemaaktDoor.medewerker")
		{
			@Override
			public IModel<?> getDataModel(IModel<MammaDownloadOnderzoekenVerzoek> rowModel)
			{
				var dataModel = super.getDataModel(rowModel);
				var medewerker = (Gebruiker) dataModel.getObject();
				return new Model<>(NaamUtil.getNaamGebruiker(medewerker));
			}
		});
		columns.add(new DateTimePropertyColumn<>(Model.of("Gewijzigd"), "gewijzigdOp", "gewijzigdOp", Constants.getDateTimeFormat()));
		columns.add(new EnumPropertyColumn<>(Model.of("Status"), "status", "status", this));
		columns.add(new ClientColumn<>("onderzoeken[0].onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client"));
		columns.add(new PropertyColumn<>(Model.of("Bsn"), "onderzoeken[0].onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon.bsn"));
		columns.add(new GeboortedatumColumn<>("onderzoeken[0].onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon"));
		columns.add(meldingKolom());
		columns.add(downloadKolom());
		columns.add(new DateTimePropertyColumn<>(Model.of("Gedownload op"), "gedownloadOp", "gedownloadOp", Constants.getDateTimeFormat()));
		columns.add(opnieuwOphalenKolom());

		verzoekFilter = ModelUtil.ccModel(uitwisselPortaalService.maakDownloadVerzoekFilter(ScreenitSession.get().getLoggedInInstellingGebruiker()));
		var dataProvider = new MammaDownloadOnderzoekenVerzoekenProvider(verzoekFilter);
		werklijst = new ScreenitDataTable<>("werklijst", columns, dataProvider, 10, Model.of("verzoek(en)"))
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

	private AbstractColumn<MammaDownloadOnderzoekenVerzoek, String> opnieuwOphalenKolom()
	{
		return new AbstractColumn<>(Model.of("Opnieuw ophalen"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaDownloadOnderzoekenVerzoek>> cellItem, String componentId, IModel<MammaDownloadOnderzoekenVerzoek> rowModel)
			{
				if (uitwisselPortaalService.zipKanGedownloadWorden(rowModel.getObject()))
				{
					cellItem.add(new AjaxImageCellPanel<>(componentId, rowModel, "icon-refresh")
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
		};
	}

	private AbstractColumn<MammaDownloadOnderzoekenVerzoek, String> downloadKolom()
	{
		return new AbstractColumn<>(Model.of("Download Zip"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaDownloadOnderzoekenVerzoek>> cellItem, String componentId, IModel<MammaDownloadOnderzoekenVerzoek> rowModel)
			{
				if (uitwisselPortaalService.zipKanGedownloadWorden(rowModel.getObject()))
				{
					cellItem.add(getDownloadCell(componentId, rowModel));
				}
				else
				{
					cellItem.add(new EmptyPanel(componentId));
				}
			}
		};
	}

	private Component getDownloadCell(String componentId, IModel<MammaDownloadOnderzoekenVerzoek> rowModel)
	{
		var downloadVerzoek = rowModel.getObject();
		var pageParameters = new PageParameters()
			.add("id", downloadVerzoek.getId())
			.add("antiCache", downloadVerzoek.getGewijzigdOp().getTime());

		return new IndicatingAjaxImageCellPanel<>(componentId, rowModel, "icon-download")
		{
			@Override
			protected void onClick(AjaxRequestTarget target, IModel<MammaDownloadOnderzoekenVerzoek> model)
			{
				uitwisselPortaalService.updateDownloadVerzoekInformatie(model.getObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				target.appendJavaScript("setTimeout(() => window.location.href='" + urlFor(downloadResourceReference, pageParameters) + "', 100);");
				target.add(werklijst);
			}
		};
	}

	private AbstractColumn<MammaDownloadOnderzoekenVerzoek, String> meldingKolom()
	{

		return new AbstractColumn<>(Model.of("Melding"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaDownloadOnderzoekenVerzoek>> cellItem, String componentId, IModel<MammaDownloadOnderzoekenVerzoek> rowModel)
			{
				BestandStatus status = rowModel.getObject().getStatus();
				if (status == BestandStatus.VERWERKT || status == BestandStatus.CRASH)
				{
					cellItem.add(new Label(componentId, maakMeldingTekst(rowModel)));
				}
				else
				{
					cellItem.add(new EmptyPanel(componentId));
				}
			}

			private String maakMeldingTekst(IModel<MammaDownloadOnderzoekenVerzoek> rowModel)
			{
				return rowModel.getObject().getOnderzoeken().stream().map(this::meldingTekstOnderzoek).collect(Collectors.joining());
			}

			private String meldingTekstOnderzoek(MammaDownloadOnderzoek downloadOnderzoek)
			{
				var onderzoek = downloadOnderzoek.getOnderzoek();
				var tekst = "Onderzoeksdatum " + Constants.getDateTimeFormat().format(onderzoek.getCreatieDatum());
				if (onderzoek.getOnderzoekType() == MammaOnderzoekType.TOMOSYNTHESE)
				{
					tekst += " (Tomosynthese)";
				}
				tekst += ": " + downloadOnderzoek.getStatusMelding();
				if (!StringUtils.trim(tekst).endsWith("."))
				{
					tekst += ". ";
				}
				if (!tekst.endsWith(" "))
				{
					tekst += " ";
				}
				return tekst;
			}
		};

	}

	private void createTimer(MammaDownloadOnderzoekenVerzoekenProvider dataProvider)
	{
		timer = new PollingAbstractAjaxTimerBehavior(Duration.of(5, ChronoUnit.SECONDS))
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
		AjaxRequestTarget target = RequestCycle.get().find(AjaxRequestTarget.class).orElseThrow();
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
		uitwisselPortaalService.beschikbareRondesVoorDownload(clientOpt.getObject())
			.stream()
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
