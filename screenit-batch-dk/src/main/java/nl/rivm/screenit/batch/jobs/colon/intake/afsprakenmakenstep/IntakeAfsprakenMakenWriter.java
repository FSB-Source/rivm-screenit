
package nl.rivm.screenit.batch.jobs.colon.intake.afsprakenmakenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.colon.intake.IntakeAfsprakenMakenConstants;
import nl.rivm.screenit.batch.model.ClientAfspraak;
import nl.rivm.screenit.dao.colon.AfspraakDefinitieDao;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.IntakeMakenLogEventRegel;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.colon.planning.VrijSlot;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.IntakeMakenLogEvent;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonHuisartsBerichtService;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;

public class IntakeAfsprakenMakenWriter implements ItemWriter<ClientAfspraak>
{

	private static final String SKIP_MELDING = "skipMelding";

	private static final Logger LOGGER = LoggerFactory.getLogger(IntakeAfsprakenMakenWriter.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private AfspraakDefinitieDao afspraakDefinitieDao;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private AfspraakService afspraakService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private StepExecution stepExecution;

	@Autowired
	private ColonHuisartsBerichtService berichtService;

	@Autowired
	private LogService logService;

	@Autowired
	private ColonDossierBaseService dossierService;

	private static final int AANTAL_DAGEN = 14;

	@Override
	public void write(List<? extends ClientAfspraak> items)
	{
		Integer intakeafspraakperiode = preferenceService.getInteger(PreferenceKey.INTAKEAFSPRAAKPERIODE.name());
		if (intakeafspraakperiode == null)
		{

			intakeafspraakperiode = AANTAL_DAGEN; 
		}
		DateTime nuPlusIntake = currentDateSupplier.getDateTime().plusDays(intakeafspraakperiode);

		ExecutionContext executionContext = stepExecution.getJobExecution().getExecutionContext();
		IntakeMakenLogEvent intakeMelding = (IntakeMakenLogEvent) executionContext.get(IntakeAfsprakenMakenConstants.RAPPORTAGEKEYINTAKE);
		Integer maxDistance = preferenceService.getInteger(PreferenceKey.MAX_AFSTAND_CLIENT_COLOSCOPIECENTRUM.name());
		int ronde = executionContext.getInt(IntakeAfsprakenMakenConstants.HUIDIGE_RONDE, 0);
		Integer maxPogingen = preferenceService.getInteger(PreferenceKey.COLON_MAX_EXTRA_POGINGEN_PLANNING_INTAKE.name());
		if (maxPogingen == null)
		{
			maxPogingen = 0;
		}

		Boolean allesVerwerkt = (Boolean) executionContext.get(IntakeAfsprakenMakenConstants.ALLE_INTAKES_VERWERKT);

		boolean maxExtraDagenBereikt = intakeMelding.getAantalExtraDagen() >= preferenceService.getInteger(PreferenceKey.COLON_MAX_EXTRA_DAGEN_PLANNING_INTAKE.name());
		for (ClientAfspraak item : items)
		{
			String bsn = null;
			Long clientId = null;
			try
			{
				if (maxDistance == null || item.getDistance() < maxDistance 
					|| ronde >= maxPogingen 
					|| maxExtraDagenBereikt)
				{
					IntakeMakenLogEventRegel logRegel = new IntakeMakenLogEventRegel();
					VrijSlot vrijSlot = item.getVrijSlot();
					if (vrijSlot != null)
					{

						ColonScreeningRonde screeningRonde = hibernateService.load(ColonScreeningRonde.class, item.getColonScreeningRondeId());
						Client client = screeningRonde.getDossier().getClient();
						GbaPersoon persoon = client.getPersoon();
						bsn = persoon.getBsn();
						clientId = client.getId();

						Gemeente gbaGemeente = persoon.getGbaAdres().getGbaGemeente();

						ColonIntakeAfspraak newAfspraak = new ColonIntakeAfspraak();
						DateTime startDatumTijd = new DateTime(vrijSlot.getStartTijd());
						LOGGER.trace("Voor client wordt een intake afspraak gemaakt (id " + clientId + ")");
						newAfspraak.setColonScreeningRonde(screeningRonde);
						newAfspraak.setClient(client);
						newAfspraak.setActief(true);
						newAfspraak.setBezwaar(false);
						newAfspraak.setDatumLaatsteWijziging(currentDateSupplier.getDate());
						newAfspraak.setAfspraaknummer(System.currentTimeMillis());
						newAfspraak.setStatus(AfspraakStatus.GEPLAND);

						newAfspraak.setAfstand(BigDecimal.valueOf(item.getDistance()));
						Kamer kamer = hibernateService.load(Kamer.class, vrijSlot.getKamerId());
						ColoscopieCentrum coloscopieCentrum = kamer.getColoscopieCentrum();
						List<AfspraakDefinitie> afspraakDefinities = afspraakDefinitieDao.getActieveActieDefinities(coloscopieCentrum);

						if (afspraakDefinities.size() != 1)
						{
							throw new IllegalStateException("Geen of te veel afspraakDefinities in IL " + coloscopieCentrum.getNaam());
						}
						AfspraakDefinitie afspraakDefinitie = afspraakDefinities.get(0);

						newAfspraak.setStartTime(vrijSlot.getStartTijd());
						newAfspraak.setEndTime(vrijSlot.getEindTijd());
						newAfspraak.setDefinition(afspraakDefinitie);
						newAfspraak.addDiscipline(afspraakDefinitie.getDisciplines().get(0));
						newAfspraak.setLocation(kamer);
						RoosterItem roosterItem = afspraakService.getRoosterBlokVoorAfspraak(newAfspraak);
						String foutMessage = "Vrij slot is intussen verwijderd/verplaatst door de intakelocatie " + createMessageContext(clientId, vrijSlot, kamer);
						for (Afspraak andereAfspraak : roosterItem.getAfspraken())
						{
							if (andereAfspraak.getStatus() == AfspraakStatus.GEPLAND)
							{
								roosterItem = null;
								foutMessage = "Er is intussen al een andere afspraak gepland op het door het alg. gekozen slot " + createMessageContext(clientId, vrijSlot, kamer);
							}
						}
						if (roosterItem == null)
						{
							throw new IllegalStateException(foutMessage);
						}
						newAfspraak.setRoosterItem(roosterItem);

						hibernateService.save(newAfspraak);

						roosterItem.getAfspraken().add(newAfspraak);
						hibernateService.saveOrUpdate(roosterItem);

						screeningRonde.setLaatsteAfspraak(newAfspraak);
						screeningRonde.getAfspraken().add(newAfspraak);
						hibernateService.saveOrUpdate(screeningRonde);

						client.getAfspraken().add(newAfspraak);
						hibernateService.saveOrUpdate(client);
						dossierService.setDatumVolgendeUitnodiging(screeningRonde.getDossier(), ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);

						BriefType type = null;
						ColonIntakeAfspraak laatsteIntakeAfspraak = null;

						if (item.getIntakeAfspraakId() != null)
						{
							laatsteIntakeAfspraak = hibernateService.get(ColonIntakeAfspraak.class, item.getIntakeAfspraakId());
						}
						if (laatsteIntakeAfspraak == null || laatsteIntakeAfspraak.getStatus() == null || AfspraakStatus.isGeannuleerd(laatsteIntakeAfspraak.getStatus()))
						{
							type = BriefType.COLON_UITNODIGING_INTAKE;
						}
						if (type != null)
						{
							ColonBrief brief = briefService.maakBvoBrief(screeningRonde, type);
							if (BriefType.COLON_UITNODIGING_INTAKE.equals(type))
							{
								brief.setIfobtTest(ColonScreeningRondeUtil.getEersteOngunstigeTest(screeningRonde));
							}
							brief.setIntakeAfspraak(newAfspraak);
							hibernateService.saveOrUpdate(brief);

							MailMergeContext context = new MailMergeContext();
							context.setClient(client);
							context.setColonUitnodiging(screeningRonde.getLaatsteUitnodiging());
							context.setIntakeAfspraak(screeningRonde.getLaatsteAfspraak());
							context.setVorigeIntakeAfspraak(laatsteIntakeAfspraak);
							berichtService.verstuurColonHuisartsBericht(client, screeningRonde, HuisartsBerichtType.ONGUNSTIGE_UITSLAG, context);
						}
						logRegel.setAfspraakId(newAfspraak.getId());

						if (maxDistance != null && item.getDistance() > maxDistance)
						{
							logService.logGebeurtenis(LogGebeurtenis.COLON_INTAKE_AFSPRAAK_BUITEN_MAX_AFSTAND, Arrays.asList(gbaGemeente.getScreeningOrganisatie()), client,
								"Afstand is " + BigDecimalUtil.decimalToString(newAfspraak.getAfstand(), 2) + "km", Bevolkingsonderzoek.COLON);
							intakeMelding.setAantalBuitenMaximaleAfstand(intakeMelding.getAantalBuitenMaximaleAfstand() + 1);
						}

						if (!nuPlusIntake.isAfter(startDatumTijd))
						{
							if (!Level.ERROR.equals(intakeMelding.getLevel()))
							{
								intakeMelding.setLevel(Level.WARNING);
							}
							LOGGER.trace("Client valt buiten vrije slot max. tijd (id " + clientId + ")");
							logRegel.setClientId(client.getId());
							logRegel.setVrijSlot(
								vrijSlot.getDatumAsString() + "," + vrijSlot.getStartTijdAsString() + "," + vrijSlot.getEindTijdAsString() + "," + vrijSlot.getKamerId());
						}
						removeMeldingForBsn(bsn);
					}
					else
					{
						logRegel.setClientId(item.getClientId());
						intakeMelding.setLevel(Level.ERROR);
					}
					logRegel.setLogEvent(intakeMelding);
					intakeMelding.getRegels().add(logRegel);
				}
				else
				{
					LOGGER.trace("Client is overgeslagen omdat de afstand naar de intakelocatie te groot was. Client-ID:" + item.getClientId() + ", afstand:" + item.getDistance());
					allesVerwerkt = false;
				}
			}
			catch (Exception e)
			{
				LOGGER.error("Fout bij aanmaken intake afspraak voor client id " + clientId, e);
				String message = e.getMessage();
				if (StringUtils.isBlank(message))
				{
					message = "Een onverwachte fout bij aanmaken intake afspraak voor client " + bsn;
				}
				if (!message.equals(SKIP_MELDING))
				{
					String huidigeMelding = null;
					if (executionContext.containsKey(IntakeAfsprakenMakenConstants.FOUT_BIJ_INTAKE_VASTLEGGEN))
					{
						huidigeMelding = executionContext.getString(IntakeAfsprakenMakenConstants.FOUT_BIJ_INTAKE_VASTLEGGEN);
					}
					if (StringUtils.isNotBlank(huidigeMelding))
					{
						if (!huidigeMelding.contains(message))
						{
							huidigeMelding += "<br>" + message;
						}
					}
					else
					{
						huidigeMelding = message;
					}
					executionContext.put(IntakeAfsprakenMakenConstants.FOUT_BIJ_INTAKE_VASTLEGGEN, huidigeMelding);
				}
			}
		}
		executionContext.put(IntakeAfsprakenMakenConstants.ALLE_INTAKES_VERWERKT, allesVerwerkt);
		hibernateService.getHibernateSession().flush();
		hibernateService.getHibernateSession().clear();
	}

	private void removeMeldingForBsn(String bsn)
	{
		ExecutionContext executionContext = stepExecution.getJobExecution().getExecutionContext();

		if (executionContext.containsKey(IntakeAfsprakenMakenConstants.FOUT_BIJ_INTAKE_VASTLEGGEN))
		{
			String huidigeMelding = executionContext.getString(IntakeAfsprakenMakenConstants.FOUT_BIJ_INTAKE_VASTLEGGEN);
			String nieuweMelding = "";
			if (StringUtils.isNotBlank(huidigeMelding))
			{
				for (String melding : huidigeMelding.split("<br>"))
				{
					if (!melding.contains(bsn))
					{
						if (StringUtils.isNotBlank(nieuweMelding))
						{
							nieuweMelding += "<br>" + melding;
						}
						else
						{
							nieuweMelding = melding;
						}
					}
				}
				executionContext.put(IntakeAfsprakenMakenConstants.FOUT_BIJ_INTAKE_VASTLEGGEN, nieuweMelding);
			}
		}
	}

	private static String createMessageContext(Long clientId, VrijSlot vrijSlot, Kamer kamer)
	{
		return "(Overgeslagen: client id " + clientId + ",Slot " + vrijSlot.getDatumAsString() + " " + vrijSlot.getStartTijdAsString() + "-" + vrijSlot.getEindTijdAsString() + " "
			+ kamer.getName() + "/" + kamer.getColoscopieCentrum().getNaam() + ")";
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}

	public void setHuisartsBerichtService(ColonHuisartsBerichtService berichtService)
	{
		this.berichtService = berichtService;
	}
}
