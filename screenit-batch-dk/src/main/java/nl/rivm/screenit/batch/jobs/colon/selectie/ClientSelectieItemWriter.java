package nl.rivm.screenit.batch.jobs.colon.selectie;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Date;
import java.util.List;
import java.util.Set;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.dao.colon.impl.ColonRestrictions;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ClientCategorieEntry;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonVooraankondiging;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.SelectieType;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.model.project.ProjectStatus;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportage;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportageEntry;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportageProjectGroepEntry;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.criterion.Projections;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemWriter;
import org.springframework.stereotype.Component;

@Component
@StepScope
@Slf4j
@RequiredArgsConstructor
public class ClientSelectieItemWriter implements ItemWriter<ClientCategorieEntry>
{

	private final HibernateService hibernateService;

	private final SimplePreferenceService simplePreferenceService;

	private final LogService logService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final BaseBriefService briefService;

	private final UitnodigingsDao uitnodigingsDao;

	private final ColonUitnodigingService uitnodigingService;

	private final ColonDossierBaseService dossierBaseService;

	private final AfspraakService afspraakService;

	private StepExecution stepExecution;

	@Override
	public void write(List<? extends ClientCategorieEntry> items)
	{
		var selectieRapportage = hibernateService.load(SelectieRapportage.class,
			stepExecution.getJobExecution().getExecutionContext().getLong(SelectieConstants.RAPPORTAGEKEYSELECTIE));
		int aantalRondesUitnodigingsbriefZonderFit = simplePreferenceService.getInteger(PreferenceKey.COLON_AANTAL_RONDES_UITNODIGINGSBRIEF_ZONDER_FIT.name());

		for (var categorieEntry : items)
		{
			var client = hibernateService.load(Client.class, categorieEntry.getClientId());

			var categorie = categorieEntry.getCategorie();

			boolean magUitnodigingMetFitMaken = ColonScreeningRondeUtil.magUitnodigingMetFitMaken(client.getColonDossier(), aantalRondesUitnodigingsbriefZonderFit)
				|| categorie != ColonUitnodigingCategorie.U1 && categorie != ColonUitnodigingCategorie.U2;

			var ronde = maakNieuweOrGeefLaatsteRonde(client, categorie, categorieEntry.getGepusht());

			if (magUitnodigingMetFitMaken)
			{
				maakNieuweUitnodiging(ronde, categorie);
			}
			else
			{
				briefService.maakBvoBrief(ronde, BriefType.COLON_UITNODIGING_ZONDER_FIT, currentDateSupplier.getDate());
			}

			rapportageBijwerken(selectieRapportage, categorieEntry, client);

		}

		hibernateService.saveOrUpdate(selectieRapportage);

		hibernateService.getHibernateSession().flush();
		hibernateService.getHibernateSession().clear();
	}

	private void rapportageBijwerken(SelectieRapportage selectieRapportage, ClientCategorieEntry categorieEntry, Client client)
	{
		var categorie = categorieEntry.getCategorie();

		if (client.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging() == null)
		{
			categorie = ColonUitnodigingCategorie.U2_4;
		}
		SelectieRapportageEntry entry = null;
		for (SelectieRapportageEntry entry2 : selectieRapportage.getEntries())
		{
			if (entry2.getColonUitnodigingCategorie().equals(categorie) && entry2.getSelectieType() == SelectieType.UITNODIGING_GEMAAKT)
			{
				entry = entry2;
				break;
			}
		}

		if (entry == null)
		{
			entry = new SelectieRapportageEntry();
			entry.setColonUitnodigingCategorie(categorie);
			entry.setAantal(0L);
			entry.setWaarvanGepusht(0L);
			entry.setRapportage(selectieRapportage);
			entry.setSelectieType(SelectieType.UITNODIGING_GEMAAKT);
			selectieRapportage.getEntries().add(entry);
		}

		entry.setAantal(entry.getAantal() + 1);
		if (Boolean.TRUE.equals(categorieEntry.getGepusht()))
		{
			entry.setWaarvanGepusht(entry.getWaarvanGepusht() + 1);
		}
		hibernateService.saveOrUpdate(entry);

		if (categorieEntry.getProjectGroepId() != null)
		{
			updateCounterProject(selectieRapportage, categorieEntry.getProjectGroepId(), client);
		}
	}

	private ColonScreeningRonde maakNieuweOrGeefLaatsteRonde(Client client, ColonUitnodigingCategorie categorie, Boolean gepushed)
	{
		ProjectClient pClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate());

		ColonScreeningRonde laatsteScreeningRonde;
		ColonDossier dossier = client.getColonDossier();
		if (categorie == ColonUitnodigingCategorie.U1 || categorie == ColonUitnodigingCategorie.U2)
		{

			if (categorie == ColonUitnodigingCategorie.U2)
			{
				heractiveerDossier(dossier);
				sluitVorigeRonde(dossier);
			}

			laatsteScreeningRonde = new ColonScreeningRonde();
			laatsteScreeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
			laatsteScreeningRonde.setStatusDatum(currentDateSupplier.getDate());
			laatsteScreeningRonde.setCreatieDatum(currentDateSupplier.getDate());
			laatsteScreeningRonde.setAangemeld(true);
			laatsteScreeningRonde.setDossier(dossier);
			if (Boolean.TRUE.equals(gepushed))
			{
				LOG.info("Client (id: '{}') gepushed", client.getId());
				laatsteScreeningRonde.setGepusht(true);
			}

			dossier.setLaatsteScreeningRonde(laatsteScreeningRonde);
			dossier.getScreeningRondes().add(laatsteScreeningRonde);

			hibernateService.saveOrUpdate(laatsteScreeningRonde);
			hibernateService.saveOrUpdate(dossier);
			dossierBaseService.setDatumVolgendeUitnodiging(dossier, ColonUitnodigingsintervalType.UITNODIGING_ONTVANGEN);

			if (pClient != null && pClient.getProject() != null)
			{
				ProjectStatus status = ProjectUtil.getStatus(pClient.getProject(), currentDateSupplier.getDate());
				if (ProjectStatus.NOG_TE_STARTEN.equals(status)
					&& pClient.getProject().getExcludeerOpenRonde().contains(Bevolkingsonderzoek.COLON))
				{
					pClient.setActief(Boolean.FALSE);
					pClient.setProjectInactiefReden(ProjectInactiefReden.INACTIVATIE_DOOR_EXCLUSIE_OPENRONDE);
					pClient.setProjectInactiefDatum(currentDateSupplier.getDate());
					hibernateService.saveOrUpdate(pClient);
				}
				if (ProjectStatus.NOG_TE_STARTEN.equals(status)
					|| ProjectStatus.ACTIEF.equals(status))
				{
					pClient.setIsUitgenodigdInProjectPeriode(Boolean.TRUE);
					hibernateService.saveOrUpdate(pClient);
				}
			}
		}
		else
		{

			laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();
			laatsteScreeningRonde.setLaatsteIFOBTTest(null);
			laatsteScreeningRonde.setLaatsteIFOBTTestExtra(null);
		}

		if (categorie.equals(ColonUitnodigingCategorie.U1))
		{
			ColonVooraankondiging vooraankondiging = new ColonVooraankondiging();
			vooraankondiging.setClient(client);
			vooraankondiging.setCreatieDatum(currentDateSupplier.getDate());

			ColonBrief brief = briefService.maakBvoBrief(laatsteScreeningRonde, BriefType.COLON_VOORAANKONDIGING);
			vooraankondiging.setBrief(brief);

			dossier.setColonVooraankondiging(vooraankondiging);
			hibernateService.saveOrUpdate(vooraankondiging);
			hibernateService.saveOrUpdate(dossier);

		}
		else
		{
			LOG.info(categorie.name() + " klaargezet voor client (id: '" + client.getId() + "')");
		}

		hibernateService.saveOrUpdate(laatsteScreeningRonde);

		verwerkLimietWaarschuwingen(laatsteScreeningRonde);

		return laatsteScreeningRonde;
	}

	private void heractiveerDossier(ColonDossier dossier)
	{

		if (DossierStatus.INACTIEF.equals(dossier.getStatus()) && Boolean.TRUE.equals(dossier.getAangemeld()))
		{
			dossier.setStatus(DossierStatus.ACTIEF);
			var volgendeUitnodiging = dossier.getVolgendeUitnodiging();
			if (volgendeUitnodiging != null && volgendeUitnodiging.getDatumVolgendeRonde() != null)
			{
				volgendeUitnodiging.setDatumVolgendeRonde(null);
			}
		}
	}

	private void sluitVorigeRonde(ColonDossier dossier)
	{
		ColonScreeningRonde eerdereScreeningRonde = dossier.getLaatsteScreeningRonde();
		if (eerdereScreeningRonde != null && eerdereScreeningRonde.getStatus() == ScreeningRondeStatus.LOPEND)
		{
			ColonIntakeAfspraak laatsteAfspraak = eerdereScreeningRonde.getLaatsteAfspraak();
			eerdereScreeningRonde.setStatus(ScreeningRondeStatus.AFGEROND);
			eerdereScreeningRonde.setStatusDatum(currentDateSupplier.getDate());
			if (laatsteAfspraak != null && laatsteAfspraak.getStatus() == AfspraakStatus.GEPLAND)
			{
				afspraakService.annuleerAfspraak(laatsteAfspraak, null, AfspraakStatus.GEANNULEERD_ONBEKEND, true);
			}
			logService.logGebeurtenis(LogGebeurtenis.RONDE_VERLOPEN, dossier.getClient(), Bevolkingsonderzoek.COLON);
			hibernateService.saveOrUpdate(eerdereScreeningRonde);
		}
	}

	private void maakNieuweUitnodiging(ColonScreeningRonde laatsteScreeningRonde, ColonUitnodigingCategorie categorie)
	{
		int vooraankondigingsPeriode = simplePreferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name());
		ColonUitnodiging nieuweUitnodiging = new ColonUitnodiging();
		nieuweUitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
		nieuweUitnodiging.setColonUitnodigingCategorie(categorie);
		nieuweUitnodiging.setCreatieDatum(currentDateSupplier.getDate());
		nieuweUitnodiging.setOnderzoeksVariant(onderzoeksVariantVoorNieuweUitnodiging(laatsteScreeningRonde));

		if (categorie == ColonUitnodigingCategorie.U1 || categorie == ColonUitnodigingCategorie.U2)
		{
			Date datumHuidigeUitnodiging = DateUtil.toUtilDate(currentDateSupplier.getLocalDate().plusDays(vooraankondigingsPeriode));
			nieuweUitnodiging.setUitnodigingsDatum(datumHuidigeUitnodiging);
		}
		else
		{ 
			nieuweUitnodiging.setUitnodigingsDatum(currentDateSupplier.getDate());
		}

		nieuweUitnodiging.setScreeningRonde(laatsteScreeningRonde);

		laatsteScreeningRonde.getUitnodigingen().add(nieuweUitnodiging);
		laatsteScreeningRonde.setLaatsteUitnodiging(nieuweUitnodiging);

		hibernateService.saveOrUpdateAll(nieuweUitnodiging, laatsteScreeningRonde);
	}

	private ColonOnderzoeksVariant onderzoeksVariantVoorNieuweUitnodiging(ColonScreeningRonde laatsteScreeningRonde)
	{
		ProjectClient projectClient = ProjectUtil.getHuidigeProjectClient(laatsteScreeningRonde.getDossier().getClient(), currentDateSupplier.getDate());
		if (ProjectUtil.hasParameterSet(projectClient, ProjectParameterKey.COLON_ONDERZOEKSVARIANT))
		{
			return ColonOnderzoeksVariant.valueOf(ProjectUtil.getParameter(projectClient.getProject(), ProjectParameterKey.COLON_ONDERZOEKSVARIANT));
		}
		return ColonOnderzoeksVariant.STANDAARD;
	}

	private void verwerkLimietWaarschuwingen(ColonScreeningRonde laatsteScreeningRonde)
	{
		int waarschuwingAantalIfobts = simplePreferenceService.getInteger(PreferenceKey.WAARSCHUWINGAANTALIFOBTS.name());
		int maximaalAantalIfobts = simplePreferenceService.getInteger(PreferenceKey.MAXIMUMAANTALIFOBTS.name());

		Client client = laatsteScreeningRonde.getDossier().getClient();
		List<ColonUitnodiging> uitnodigingen = laatsteScreeningRonde.getUitnodigingen();
		int totaalAantalVerstuurdeUitnodigingen = uitnodigingen.size();

		List<Instelling> dashboardOrganisaties = List.of(client.getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie());
		if (totaalAantalVerstuurdeUitnodigingen >= waarschuwingAantalIfobts && totaalAantalVerstuurdeUitnodigingen < maximaalAantalIfobts)
		{
			LogEvent logEvent = new LogEvent();
			logEvent.setMelding("Totaal aantal uitnodigingen aangemaakt voor deze client: " + totaalAantalVerstuurdeUitnodigingen + " (ingestelde waarschuwingslimiet: "
				+ waarschuwingAantalIfobts + ")");
			logEvent.setLevel(Level.WARNING);
			logService.logGebeurtenis(LogGebeurtenis.COLON_LIMIET_UITNODIGINGEN, dashboardOrganisaties,
				logEvent, client, Bevolkingsonderzoek.COLON);
			updateLimietIfobts(SelectieConstants.COLONSELECTIEWAARSCHUWINGIFOBTS);

		}
		else if (totaalAantalVerstuurdeUitnodigingen >= maximaalAantalIfobts)
		{
			LogEvent logEvent = new LogEvent();
			logEvent.setMelding(
				"Totaal aantal uitnodigingen aangemaakt voor deze client: " + totaalAantalVerstuurdeUitnodigingen + " (ingestelde maximale limiet: " + maximaalAantalIfobts + ")");
			logEvent.setLevel(Level.ERROR);
			logService.logGebeurtenis(LogGebeurtenis.COLON_LIMIET_UITNODIGINGEN, dashboardOrganisaties,
				logEvent, client, Bevolkingsonderzoek.COLON);
			updateLimietIfobts(SelectieConstants.COLONSELECTIEMAXIMAALIFOBTS);
		}
	}

	private void updateLimietIfobts(String constant)
	{
		int aantalClienten = getExecutionContext().getInt(constant);
		aantalClienten++;
		getExecutionContext().putInt(constant, aantalClienten);
	}

	private void updateCounterProject(SelectieRapportage selectieRapportage, long projectGroepId, Client client)
	{
		SelectieRapportageProjectGroepEntry projectGroepEntry = null;

		for (SelectieRapportageProjectGroepEntry entry2 : selectieRapportage.getProjectGroepen())
		{
			if (entry2.getProjectGroep().getId().equals(projectGroepId) && entry2.getSelectieType() == SelectieType.UITNODIGING_GEMAAKT)
			{
				projectGroepEntry = entry2;
				break;
			}
		}

		if (projectGroepEntry != null) 
		{
			projectGroepEntry.setAantal(projectGroepEntry.getAantal() + 1);
			if (projectGroepEntry.getClientenNogTeGaan() > 0)
			{
				projectGroepEntry.setClientenNogTeGaan(projectGroepEntry.getClientenNogTeGaan() - 1);
			}
			if (client.getColonDossier().getLaatsteScreeningRonde().isGepusht())
			{
				projectGroepEntry.setWaarvanGepusht(projectGroepEntry.getWaarvanGepusht() + 1);
			}
			hibernateService.saveOrUpdate(projectGroepEntry);
		}
		else 
		{
			ProjectGroep projectGroep = hibernateService.get(ProjectGroep.class, projectGroepId);

			Integer minimaleLeeftijd = simplePreferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name());
			if (minimaleLeeftijd == null)
			{
				throw new IllegalStateException("Minimale leeftijd colonscreening op de parameterisatie pagina is niet gezet.");
			}

			Integer maximaleLeeftijd = simplePreferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
			if (maximaleLeeftijd == null)
			{
				throw new IllegalStateException("Maximale leeftijd colonscreening op de parameterisatie pagina is niet gezet");
			}
			Integer wachttijdVerzendenPakket = simplePreferenceService.getInteger(PreferenceKey.WACHTTIJD_VERZENDEN_PAKKET_TWEE_OP_EEN_ADRES.name());
			if (wachttijdVerzendenPakket == null)
			{
				throw new IllegalStateException("Wachttijd verzenden pakket bij 2 op 1 adres op de parameterisatie pagina is niet gezet");
			}

			Set<Integer> alleGeboortejarenVanActiveCohorten = uitnodigingService.getAlleGeboortejarenTotMetHuidigJaar();

			var criteria = ColonRestrictions.getQueryVooraankondigen(hibernateService.getHibernateSession(), null, new ArrayList<>(alleGeboortejarenVanActiveCohorten),
				true, minimaleLeeftijd, maximaleLeeftijd, projectGroep.getId(), null, currentDateSupplier.getLocalDate());
			criteria.setProjection(Projections.rowCount());
			Long aantalNogTeGaan = (Long) criteria.uniqueResult();
			int aantalWerkDagen = 0;
			Date uitnodigenVoorDKvoor = projectGroep.getUitnodigenVoorDKvoor();
			if (uitnodigenVoorDKvoor != null)
			{
				aantalWerkDagen = DateUtil.getDaysBetweenIgnoreWeekends(currentDateSupplier.getDateMidnight(), uitnodigenVoorDKvoor, false) - 1;
			}

			SelectieRapportageProjectGroepEntry entry = new SelectieRapportageProjectGroepEntry();
			entry.setRapportage(selectieRapportage);
			entry.setClientenNogTeGaan(aantalNogTeGaan - 1); 
			entry.setAantal(1L);
			entry.setWaarvanGepusht(0L);
			if (client.getColonDossier().getLaatsteScreeningRonde().isGepusht())
			{
				entry.setWaarvanGepusht(1L);
			}
			entry.setDagenNogTeGaan(aantalWerkDagen);

			entry.setProjectGroep(projectGroep);
			entry.setSelectieType(SelectieType.UITNODIGING_GEMAAKT);
			selectieRapportage.getProjectGroepen().add(entry);
			hibernateService.saveOrUpdate(entry);
		}
		hibernateService.saveOrUpdate(selectieRapportage);
	}

	private ExecutionContext getExecutionContext()
	{
		return stepExecution.getJobExecution().getExecutionContext();
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}
}
