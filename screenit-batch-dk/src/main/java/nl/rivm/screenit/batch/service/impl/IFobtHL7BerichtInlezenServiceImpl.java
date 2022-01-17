package nl.rivm.screenit.batch.service.impl;

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

import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.batch.service.IFobtHL7BerichtInlezenService;
import nl.rivm.screenit.dao.KwaliteitscontroleDao;
import nl.rivm.screenit.dao.colon.IFOBTResultDao;
import nl.rivm.screenit.dao.colon.IFobtDao;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.IFOBTResult;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.SKMLControleBarcode;
import nl.rivm.screenit.model.colon.berichten.ColonIFobtHL7BerichtWrapper;
import nl.rivm.screenit.model.colon.berichten.ColonIFobtUitslagBericht;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.rivm.screenit.model.colon.enums.IFOBTUitslagType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.IfobtVerwerkingBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.verwerkingverslag.IfobtVerwerkingRapportage;
import nl.rivm.screenit.model.verwerkingverslag.IfobtVerwerkingRapportageEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.IFOBTTestUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.DefaultHapiContext;
import ca.uhn.hl7v2.HapiContext;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;
import ca.uhn.hl7v2.parser.Parser;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class IFobtHL7BerichtInlezenServiceImpl implements IFobtHL7BerichtInlezenService
{

	private static final Logger LOG = LoggerFactory.getLogger(IFobtHL7BerichtInlezenServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private IFOBTResultDao ifobtResultDao;

	@Autowired
	private LogService logService;

	@Autowired
	private IFobtDao ifobtDao;

	@Autowired
	private KwaliteitscontroleDao kwaliteitscontroleDao;

	@Override
	public List<ColonIFobtUitslagBericht> getAlleNietVerwerkteIFobtBerichten()
	{
		return ifobtResultDao.getAlleNietVerwerkteHpvBerichten();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwerkOntvangenIFobtBericht(ColonIFobtUitslagBericht bericht)
	{
		IfobtVerwerkingBeeindigdLogEvent verwerkingLogEvent = new IfobtVerwerkingBeeindigdLogEvent();
		IfobtVerwerkingRapportage rapportage = new IfobtVerwerkingRapportage();
		verwerkingLogEvent.setRapportage(rapportage);
		hibernateService.saveOrUpdate(rapportage);
		hibernateService.saveOrUpdate(verwerkingLogEvent);

		boolean sentinelTestOntvangen = false;
		boolean interneTestOntvangen = false;
		boolean externeTestOntvangen = false;
		try
		{
			IFOBTBestand bestand = null;

			String ifobtBericht = bericht.getHl7Bericht();
			HapiContext context = new DefaultHapiContext();

			context.getExecutorService();
			Parser p = context.getPipeParser();
			Message hapiMsg = p.parse(ifobtBericht);

			ColonIFobtHL7BerichtWrapper wrapper = new ColonIFobtHL7BerichtWrapper((OUL_R22) hapiMsg);
			IfobtVerwerkingRapportageEntry verslagEntry = new IfobtVerwerkingRapportageEntry();

			rapportage.setDatumVerwerking(currentDateSupplier.getDate());
			verslagEntry.setBestandsNaam(bericht.getMessageId());
			verslagEntry.setRapportage(rapportage);

			String berichtVerwerkenMelding = "Start met ontvangen van uitslagen in FIT HL7 bericht: " + bericht.getMessageId();
			LOG.info(berichtVerwerkenMelding);
			logService.logGebeurtenis(LogGebeurtenis.IFOBT_INLEZEN_GESTART, (Account) null, null, berichtVerwerkenMelding, Bevolkingsonderzoek.COLON);
			for (IFOBTResult ifobtResult : wrapper.getResults())
			{
				try
				{
					boolean onbekendeBarcode = false;
					String barcode = ifobtResult.getSid();
					LOG.info("Barcode verwerken: " + barcode);
					IFOBTTest ifobtTest = ifobtDao.getIfobtTest(barcode);

					IFOBTUitslag uitslag = new IFOBTUitslag();
					uitslag.setAnalyseDatum(ifobtResult.getDateTimeResult());
					uitslag.setBarcode(barcode);
					if (ifobtTest == null || ifobtTest.getType().equals(IFOBTType.STUDIE))
					{
						SKMLControleBarcode skmlBarcode = kwaliteitscontroleDao.getSkmlBarcode(barcode);

						if (skmlBarcode == null)
						{
							String melding = "FIT of controle buis met barcode " + barcode + " in bestand " + ifobtResult.getBestandsNaam() + " bestaat niet.";
							logService.logGebeurtenis(LogGebeurtenis.IFOBT_ONBEKENDE_BARCODE, null, melding, Bevolkingsonderzoek.COLON);
							LOG.warn(melding);

							onbekendeBarcode = true;
						}
						else
						{
							uitslag.setType(skmlBarcode.getType());
							switch (skmlBarcode.getType())
							{
							case SENTINEEL:
								sentinelTestOntvangen = true;
								break;
							case INTERN:
								interneTestOntvangen = true;
								break;
							case EXTERN:
								externeTestOntvangen = true;
								break;
							default:
								break;
							}
						}
					}
					else
					{
						uitslag.setType(IFOBTUitslagType.CLIENT);
						logAction(ifobtTest);
					}
					bestand = createOrGetBestand(ifobtResult);
					verslagEntry.setIfobtBestandId(bestand.getId());

					if (!onbekendeBarcode)
					{
						uitslag.setBestand(bestand);
						uitslag.setUitslag(new BigDecimal(ifobtResult.getResultValue()));
						hibernateService.saveOrUpdate(uitslag);

						if (!IFOBTUitslagType.CLIENT.equals(uitslag.getType()))
						{
							bestand.setAantalControleUitslagen(bestand.getAantalControleUitslagen() + 1);
						}
					}
					verslagEntry.setAantalVerwerkingen(verslagEntry.getAantalVerwerkingen() + 1);
					hibernateService.saveOrUpdate(verslagEntry);
					hibernateService.saveOrUpdate(rapportage);
				}
				catch (Exception e)
				{
					String melding = "Onbekende fout in regel met barcode " + ifobtResult.getSid() + "\n";
					melding += e.getMessage();

					LOG.warn("Fout bij verwerking van uitslag regel ", e);
					logWarning(bericht, melding, verwerkingLogEvent);
				}
			}
			logSkmlTestOntvangen(sentinelTestOntvangen, interneTestOntvangen, externeTestOntvangen);
			bestand.setStatus(IFOBTBestandStatus.INGELEZEN);
			hibernateService.saveOrUpdate(bestand);
			bericht.setStatusDatum(currentDateSupplier.getDate());
			bericht.setStatus(BerichtStatus.VERWERKT);
			hibernateService.saveOrUpdate(bericht);
			hibernateService.saveOrUpdate(verwerkingLogEvent);
			logService.logGebeurtenis(LogGebeurtenis.IFOBT_INLEZEN_AFGEROND, verwerkingLogEvent, Bevolkingsonderzoek.COLON);
		}
		catch (Exception e)
		{
			LOG.warn("Fout bij verwerking van HL7 bericht", e);
			bericht.setStatus(BerichtStatus.FOUT);
			bericht.setStatusDatum(currentDateSupplier.getDate());
			hibernateService.saveOrUpdate(bericht);
			logError(bericht, e.getMessage(), verwerkingLogEvent);
		}
	}

	private void logSkmlTestOntvangen(boolean sentinelTestOntvangen, boolean interneTestOntvangen, boolean externeTestOntvangen)
	{
		if (sentinelTestOntvangen)
		{
			logService.logGebeurtenis(LogGebeurtenis.SENTINEL_ONTVANGEN, (Account) null, Bevolkingsonderzoek.COLON);
		}
		if (interneTestOntvangen)
		{
			logService.logGebeurtenis(LogGebeurtenis.INTERNE_TEST_ONTVANGEN, (Account) null, Bevolkingsonderzoek.COLON);
		}
		if (externeTestOntvangen)
		{
			logService.logGebeurtenis(LogGebeurtenis.EXTERNE_TEST_ONTVANGEN, (Account) null, Bevolkingsonderzoek.COLON);
		}
	}

	private IFOBTBestand createOrGetBestand(IFOBTResult ifobtResult) throws IOException
	{
		IFOBTBestand bestand = ifobtDao.getIfobtBestand(ifobtResult.getBestandsNaam());
		if (bestand == null)
		{
			bestand = new IFOBTBestand();
			bestand.setStatus(IFOBTBestandStatus.NIEUW);
			bestand.setStatusDatum(currentDateSupplier.getDate());
			bestand.setInstumentId(ifobtResult.getInstrumentID());
			String labID = ifobtResult.getLabID();
			IFobtLaboratorium iFobtLaboratorium = ifobtResultDao.getIFobtLaboratorium(labID);
			if (iFobtLaboratorium == null)
			{
				String melding = "FIT laboratorium met id " + labID + " in bestand " + ifobtResult.getBestandsNaam() + " bestaat niet (bestand overgeslagen).";
				throw new IllegalStateException(melding);
			}
			bestand.setLaboratorium(iFobtLaboratorium);

			bestand.setNaamBestand(ifobtResult.getBestandsNaam());
			bestand.setPathBestand("");

			hibernateService.saveOrUpdate(bestand);

		}
		return bestand;
	}

	private void logAction(IFOBTTest ifobt)
	{
		Client client = IFOBTTestUtil.getUitnodiging(ifobt).getScreeningRonde().getDossier().getClient();
		logService.logGebeurtenis(LogGebeurtenis.IFOBT_ONTVANGEN, client, "barcode: " + ifobt.getBarcode(), Bevolkingsonderzoek.COLON);
	}

	private void logWarning(ColonIFobtUitslagBericht ontvangenBericht, String message, IfobtVerwerkingBeeindigdLogEvent verwerkingLogEvent)
	{
		if (!BerichtStatus.FOUT.equals(ontvangenBericht.getStatus()))
		{
			ontvangenBericht.setStatus(BerichtStatus.WAARSCHUWING);
		}
		hibernateService.saveOrUpdate(ontvangenBericht);
		IFobtLaboratorium laboratorium = ontvangenBericht.getLaboratorium();
		String melding = "Uitslag in FIT HL7 bericht (messageID: " + ontvangenBericht.getMessageId() + ") kon niet worden verwerkt. (" + message + ")";
		addMelding(verwerkingLogEvent, melding);
	}

	private void addMelding(IfobtVerwerkingBeeindigdLogEvent verwerkingLogEvent, String melding)
	{
		String huidigeMelding = verwerkingLogEvent.getMelding();
		if (melding == null)
		{
			melding = "Er is een onbekende fout opgetreden bij de verwerking van een uitslag. Neem contact op met de helpdesk.";
		}
		if (StringUtils.isBlank(huidigeMelding))
		{
			huidigeMelding = melding;
		}
		else if (!huidigeMelding.contains(melding))
		{
			huidigeMelding += "<br>" + melding;
		}
		verwerkingLogEvent.setMelding(huidigeMelding);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void logError(ColonIFobtUitslagBericht ontvangenBericht, String message, IfobtVerwerkingBeeindigdLogEvent verwerkingLogEvent)
	{
		ontvangenBericht.setStatus(BerichtStatus.FOUT);
		hibernateService.saveOrUpdate(ontvangenBericht);
		IFobtLaboratorium laboratorium = ontvangenBericht.getLaboratorium();
		String melding = "FIT HL7 Bericht (messageID: " + ontvangenBericht.getMessageId() + ") kon niet worden verwerkt. (" + message + ")";
		logging(LogGebeurtenis.IFOBT_INLEZEN_AFGEROND, laboratorium, Level.ERROR,
			melding);
		if (verwerkingLogEvent != null)
		{
			addMelding(verwerkingLogEvent, melding);
		}
	}

	private String logging(LogGebeurtenis gebeurtenis, IFobtLaboratorium laboratorium, Level level, String melding)
	{
		LogEvent event = new LogEvent();
		event.setLevel(level);
		event.setMelding(melding);
		List<Instelling> instellingen = addRivmInstelling(new ArrayList<>());
		if (laboratorium != null)
		{
			melding += " Laboratorium: " + laboratorium.getNaam();
		}
		logService.logGebeurtenis(gebeurtenis, instellingen, event, Bevolkingsonderzoek.COLON);
		return melding;
	}

	private List<Instelling> addRivmInstelling(List<Instelling> instellingen)
	{
		List<Rivm> rivm = hibernateService.loadAll(Rivm.class);
		List<Instelling> rivmInstellingen = new ArrayList<>(rivm);
		instellingen.addAll(rivmInstellingen);
		return instellingen;
	}

}
