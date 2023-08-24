package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.lang.reflect.InvocationTargetException;
import java.util.Date;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.dao.GbaDao;
import nl.rivm.screenit.batch.jobs.generalis.gba.exception.GbaImportException;
import nl.rivm.screenit.batch.jobs.generalis.gba.wrappers.GbaValidatieWrapper;
import nl.rivm.screenit.batch.service.GbaService;
import nl.rivm.screenit.dao.CoordinatenDao;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.RedenOpnieuwAanvragenClientgegevens;
import nl.rivm.screenit.model.TijdelijkGbaAdres;
import nl.rivm.screenit.model.enums.DatumPrecisie;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.GbaVraagType;
import nl.rivm.screenit.model.enums.IndicatieGeheim;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.gba.GbaFoutCategorie;
import nl.rivm.screenit.model.gba.GbaFoutRegel;
import nl.rivm.screenit.model.gba.GbaMutatie;
import nl.rivm.screenit.model.gba.GbaVerwerkingEntry;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.model.gba.Land;
import nl.rivm.screenit.model.gba.Nationaliteit;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.TransgenderService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.gba.vertrouwdverbonden.model.GbaPartnerschap;
import nl.topicuszorg.gba.vertrouwdverbonden.model.Record;
import nl.topicuszorg.gba.vertrouwdverbonden.model.Vo107Bericht;
import nl.topicuszorg.gba.vertrouwdverbonden.model.enums.GbaRubriek;
import nl.topicuszorg.gba.vertrouwdverbonden.model.enums.Vo107_ArecordVeld;
import nl.topicuszorg.gba.vertrouwdverbonden.model.enums.VoxBrecordVeld;
import nl.topicuszorg.gba.vertrouwdverbonden.model.utils.VoxHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.NaamGebruik;
import nl.topicuszorg.util.postcode.PostcodeFormatter;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import com.google.common.base.Strings;

@Service
@Slf4j
public class GbaServiceImpl implements GbaService
{
	private static final String WA11 = "Wa11";

	@Autowired
	private ClientService clientService;

	@Autowired
	private LogService logService;

	@Autowired
	private GbaDao gbaDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CoordinatenDao coordinatenDao;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private TransgenderService transgenderService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void importVo107Bericht(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog) throws GbaImportException
	{
		Client client = null;
		try
		{

			String eref = bericht.getString(Vo107_ArecordVeld.EREF);
			LOG.debug("Verwerken vo107-bericht met EREF: {}", eref);

			if (bericht.getBerichtType().equalsIgnoreCase("dt01") || bericht.getBerichtType().equalsIgnoreCase("dw01"))
			{

				verwerkTabelRegel(bericht);
			}
			else
			{
				String bsn = bericht.getBsn();
				String anummerARecord = bericht.getString(Vo107_ArecordVeld.ANR);
				String oorspronkelijkBsn = bericht.getOorspronkelijkBsn();

				if (Strings.isNullOrEmpty(bsn))
				{
					GbaFoutRegel foutRegel = new GbaFoutRegel();
					String foutString = "Bericht geskipt: Geen BSN gevonden in A of B records. EREF: " + eref;
					foutRegel.setFout(foutString);
					foutRegel.setFoutCategorie(GbaFoutCategorie.INHOUDELIJK_ERNGSTIG);
					foutRegel.setVerwerkingsLog(verwerkingLog);
					verwerkingLog.getFouten().add(foutRegel);
					logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_GEEN_BSN, clientService.getScreeningOrganisatieVan(client), null, null, foutString);

					return;
				}

				if (!Strings.isNullOrEmpty(oorspronkelijkBsn))
				{
					client = clientService.getClientByBsn(oorspronkelijkBsn);
					Client clientMetNieuwBsn = clientService.getClientByBsn(bsn);
					Client afgevoerdeClientMetNieuwBsn = null;

					if (clientMetNieuwBsn == null)
					{
						afgevoerdeClientMetNieuwBsn = clientService.getLaatstAfgevoerdeClient(bsn);
					}

					boolean oorspronkelijkeClientHeeftDossier = client != null && clientService.heeftDossierMetRondeOfAfmelding(client);
					boolean afgevoerdeClientMetNieuwBsnHeeftDossier = afgevoerdeClientMetNieuwBsn != null
						&& clientService.heeftDossierMetRondeOfAfmelding(afgevoerdeClientMetNieuwBsn);

					if (!bsn.equals(oorspronkelijkBsn) && !oorspronkelijkeClientHeeftDossier && afgevoerdeClientMetNieuwBsnHeeftDossier)
					{
						if (client != null)
						{
							String anummerAfgevoerd = afgevoerdeClientMetNieuwBsn.getPersoon().getAnummer();
							String clientAnummer = client.getPersoon().getAnummer();
							wisselAnummers(afgevoerdeClientMetNieuwBsn, client);
							LOG.info("Anummer wordt aangepast van {} naar {} voor afgevoerde clientId: {}. EREF: {}", anummerAfgevoerd, clientAnummer,
								afgevoerdeClientMetNieuwBsn.getId(), eref);
							verwijderClient(client, verwerkingLog, eref, anummerARecord, false);
						}
						client = afgevoerdeClientMetNieuwBsn;
					}
					else if (clientMetNieuwBsn != null && client != null && !bsn.equals(oorspronkelijkBsn))
					{
						String foutmelding = "Bericht geskipt: Wijziging van bsn van " + oorspronkelijkBsn + " naar " + bsn
							+ " niet mogelijk, omdat er al andere client met het nieuwe bsn bestaat (anummer " + clientMetNieuwBsn.getPersoon().getAnummer() + "). "
							+ getFoutmelding(bericht, verwerkingLog, client);
						logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_BSN_NIET_OVEREEN, clientService.getScreeningOrganisatieVan(clientMetNieuwBsn), clientMetNieuwBsn,
							foutmelding);
						createFout(null, verwerkingLog, foutmelding, GbaFoutCategorie.OVERIG);

						return;
					}

				}
				else
				{
					client = clientService.getClientByBsn(bsn); 
				}

				if (client == null)
				{
					client = clientService.getClientByBsnFromNg01Bericht(bsn, anummerARecord);
				}

				boolean isVerwijderBericht = "Ng01".equalsIgnoreCase(bericht.getBerichtType());
				boolean clientIsVerwijderd = client != null && GbaStatus.AFGEVOERD.equals(client.getGbaStatus());
				if (client != null)
				{
					GbaValidatieWrapper validatieWrapper = valideerGevondenClient(client, bericht, verwerkingLog, isVerwijderBericht, clientIsVerwijderd);
					clientIsVerwijderd = validatieWrapper.isClientIsVerwijderd();
					if (validatieWrapper.isStopVerwerking())
					{
						return;
					}
					if (clientIsVerwijderd && !isVerwijderBericht)
					{
						client = null;
					}
				}

				if (client == null && bericht.isVerstrekking())
				{
					client = clientService.getLaatstAfgevoerdeClient(bsn);
					if (client != null && !anummerARecord.equals(client.getPersoon().getAnummer()))
					{
						if (verstrekkingErrorClientMetAnderAnummerBestaatAl(bericht, verwerkingLog, client))
						{
							return;
						}
						client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
						client.getPersoon().setBsn(bsn);
						hibernateService.saveOrUpdate(client);
						hibernateService.getHibernateSession().flush();
						String logmelding = "Client is heractiveerd met anummer wijziging naar: " + getStringUitBericht(bericht, GbaRubriek.PERS_A_NUMMER);
						logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_HERACTIVATIE, clientService.getScreeningOrganisatieVan(client), client, logmelding);
					}
				}

				if (bericht.isVerstrekking())
				{
					verwerkVerstrekking(bericht, verwerkingLog, client);
				}
				else if (bericht.isMutatie())
				{
					verwerkMutatie(bericht, verwerkingLog, bsn, client);
				}
				else if ("Null".equalsIgnoreCase(bericht.getBerichtType()))
				{
					verwerkNullBericht(bsn, client, bericht);
				}
				else if (isVerwijderBericht)
				{

					if (client != null && clientIsVerwijderd)
					{
						String foutmelding = "Bericht geskipt: Ng01 bericht maar client is reeds eerder verwijderd. " + getFoutmelding(bericht, verwerkingLog, client);
						logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_SKIP, clientService.getScreeningOrganisatieVan(client), client, foutmelding);
						createFout(null, verwerkingLog, foutmelding, GbaFoutCategorie.OVERIG);
					}
					else if (client != null)
					{
						verwijderClient(client, verwerkingLog, eref, anummerARecord, true);
					}
					else
					{
						createFout(null, verwerkingLog, "Ng01 voor onbekende burger ontvangen, EREF: " + eref, GbaFoutCategorie.INHOUDELIJK);
					}
				}
				else
				{
					LOG.error("Bericht niet verwerkt, onbekend bericht: " + bericht.getBerichtType() + " EREF: " + eref);
				}
			}
			if (client != null)
			{
				clientService.actiesNaUpdateWithGba(client);
			}
			hibernateService.getHibernateSession().flush();
		}
		catch (Exception e)
		{
			String foutmelding = "Berichtverwerking gestopt vanwege fout door bericht: " + getFoutmelding(bericht, verwerkingLog, client);
			LOG.error(foutmelding, e);

			TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();

			GbaImportException exception = new GbaImportException(foutmelding, e);
			if (client != null)
			{
				exception.setClientId(client.getId());
			}
			throw exception;
		}

	}

	private void wisselAnummers(Client clientA, Client clientB)
	{
		String anummerClientA = clientA.getPersoon().getAnummer();
		String anummerClientB = clientB.getPersoon().getAnummer();
		clientA.getPersoon().setAnummer(null);
		clientB.getPersoon().setAnummer(null);
		hibernateService.saveOrUpdate(clientA);
		hibernateService.saveOrUpdate(clientB);
		hibernateService.getHibernateSession().flush();
		clientA.getPersoon().setAnummer(anummerClientB);
		clientB.getPersoon().setAnummer(anummerClientA);
		hibernateService.saveOrUpdate(clientA);
		hibernateService.saveOrUpdate(clientB);
		hibernateService.getHibernateSession().flush();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRES_NEW)
	public void logGbaImportError(GbaImportException e, GbaVerwerkingsLog verwerkingLog)
	{
		Client client = null;
		if (e.getClientId() != null)
		{
			client = hibernateService.get(Client.class, e.getClientId());
		}

		logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_FOUT, clientService.getScreeningOrganisatieVan(client), client,
			e.getMessage());
		createFout(null, verwerkingLog, e.getMessage(), GbaFoutCategorie.PROCES);
	}

	private void verwijderClient(Client client, GbaVerwerkingsLog verwerkingLog, String eref, String anummerARecord, boolean aantalBurgersBijwerken)
	{
		String oudeClientBsn = client.getPersoon().getBsn();
		String nieuweClientBsn = clientService.getVoorNg01EenNieuweBsn(oudeClientBsn);
		if (nieuweClientBsn != null)
		{
			plaatsBSNGewijzigdMarker(client, oudeClientBsn, nieuweClientBsn);
			client.getPersoon().setBsn(nieuweClientBsn);
		}
		if (aantalBurgersBijwerken)
		{
			verwerkingLog.setAantalBijgewerkteBugers(verwerkingLog.getAantalBijgewerkteBugers() + 1);
		}
		client.setGbaStatus(GbaStatus.AFGEVOERD);
		hibernateService.saveOrUpdate(client);
		hibernateService.getHibernateSession().flush();

		String goedeString = "Bericht Verwerkt: bsn aangepast naar " + nieuweClientBsn + ", EREF " + eref + ", a-nummer uit bericht: " + anummerARecord
			+ ", anummer afgevoerde cliënt: " + client.getPersoon().getAnummer();
		logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_VERWIJDERD_VAN_PERSOONSLIJST, clientService.getScreeningOrganisatieVan(client), client, goedeString);

		if (getScreeningOrganisatie(client) != null && aantalBurgersBijwerken)
		{
			GbaVerwerkingEntry verwerkingEntry = getOrCreateEntry(verwerkingLog, client);
			verwerkingEntry.setAantalBijgewerkteBugers(verwerkingEntry.getAantalBijgewerkteBugers() + 1);
		}
	}

	private GbaValidatieWrapper valideerGevondenClient(Client client, Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog, boolean isVerwijderBericht,
		boolean clientIsVerwijderd)
	{
		String anummerARecord = bericht.getString(Vo107_ArecordVeld.ANR);
		String bsn = bericht.getBsn();

		if (client != null)
		{

			if (GbaStatus.BEZWAAR.equals(client.getGbaStatus()))
			{
				String string = "Bericht genegeerd: Client heeft bezwaar gemaakt tegen BRP. " + getFoutmelding(bericht, verwerkingLog, client);
				logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_BEZWAAR, clientService.getScreeningOrganisatieVan(client), client, string);
				return GbaValidatieWrapper.stopVerwerking(clientIsVerwijderd);
			}

			registreerMutatie(client, bericht);

			boolean isWijzigAnummerBericht = WA11.equalsIgnoreCase(bericht.getBerichtType());
			String anummerClient = client.getPersoon().getAnummer();

			if (clientIsVerwijderd && !isVerwijderBericht && anummerClient != null && 
				(isWijzigAnummerBericht || anummerClient.equals(anummerARecord)))
			{
				if (bericht.isMutatie() || bericht.isVerstrekking())
				{
					client.getPersoon().setBsn(bsn);
					client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
					String foutString =
						"Verstrekking of mutatie bericht ontvangen voor burger die van persoonslijst verwijderd is. Verwijderindicatie wordt nu weer opgeheven en het juiste bsn teruggezet. "
							+ getFoutmelding(bericht, verwerkingLog, client) + ", berichttype " + bericht.getBerichtType();
					clientIsVerwijderd = false;
					logService.logGebeurtenis(LogGebeurtenis.GBA_OPHEFFEN_VERWIJDERINDICATIE, clientService.getScreeningOrganisatieVan(client), client, foutString);
				}
				else
				{
					GbaFoutRegel foutRegel = new GbaFoutRegel();
					String foutString = "Bericht geskipt: Bericht binnengekregen voor burger die van persoonslijst verwijderd is, "
						+ getFoutmelding(bericht, verwerkingLog, client) + ", berichttype " + bericht.getBerichtType();
					foutRegel.setFout(foutString);
					logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_SKIP, clientService.getScreeningOrganisatieVan(client), client, foutString);
					foutRegel.setFoutCategorie(GbaFoutCategorie.INHOUDELIJK_ERNGSTIG);
					foutRegel.setClient(client.getId());
					foutRegel.setVerwerkingsLog(verwerkingLog);
					verwerkingLog.getFouten().add(foutRegel);

					return GbaValidatieWrapper.stopVerwerking(clientIsVerwijderd);
				}
			}

			String anummerBRecord = getStringUitBericht(bericht, GbaRubriek.PERS_A_NUMMER);
			if (!clientIsVerwijderd && anummerClient != null &&
				isWijzigAnummerBericht && !anummerClient.equals(anummerBRecord) ||
				!isWijzigAnummerBericht && !anummerClient.equals(anummerARecord))
			{
				GbaFoutRegel foutRegel = new GbaFoutRegel();
				String foutString = "Bericht geskipt: A-nummer in header van bericht en a-nummer van client (gevonden op basis van bsn) komen niet overeen. ";
				if (isWijzigAnummerBericht)
				{
					foutString = "Bericht geskipt: A-nummer in de b-record van bericht(Wa11) en a-nummer van client (gevonden op basis van bsn) komen niet overeen.";
				}
				logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_SKIP, clientService.getScreeningOrganisatieVan(client), client, foutString);
				foutString += getFoutmelding(bericht, verwerkingLog, client);
				foutRegel.setFout(foutString);
				foutRegel.setFoutCategorie(GbaFoutCategorie.INHOUDELIJK_ERNGSTIG);
				foutRegel.setClient(client.getId());
				foutRegel.setVerwerkingsLog(verwerkingLog);
				verwerkingLog.getFouten().add(foutRegel);

				return GbaValidatieWrapper.stopVerwerking(clientIsVerwijderd);
			}

		}
		return new GbaValidatieWrapper(clientIsVerwijderd, false);
	}

	protected String getFoutmelding(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog, Client client)
	{
		String bestandsnaam = "onbekend";
		if (!verwerkingLog.getBestanden().isEmpty())
		{
			bestandsnaam = verwerkingLog.getBestanden().get(verwerkingLog.getBestanden().size() - 1).getNaam();
		}

		Date geboorteDatum = getDateUitBericht(bericht, GbaRubriek.PERS_GEBOORTEDATUM);
		String foutmelding = "Record met eref " + bericht.getString(Vo107_ArecordVeld.EREF) + " in bestand " + bestandsnaam + ", bsn uit bericht: "
			+ bericht.getString(Vo107_ArecordVeld.SOFINR) + ", a-nummer uit bericht: " + bericht.getString(Vo107_ArecordVeld.ANR);

		boolean gebDatumAdded = false;
		if (client != null && client.getPersoon() != null && client.getPersoon().getGeboortedatum() != null)
		{
			foutmelding += ". Gegevens gevonden in client, bsn: " + client.getPersoon().getBsn() + ", a-nummer: " + client.getPersoon().getAnummer();

			if (client.getPersoon().getGeboortedatum() != null)
			{
				foutmelding += ", geb. datum : " + DateUtil.formatShortDate(client.getPersoon().getGeboortedatum());
				gebDatumAdded = true;
			}
		}

		if (!gebDatumAdded)
		{
			foutmelding += ", geb. datum uit bericht: ";
			if (geboorteDatum != null)
			{
				foutmelding += DateUtil.formatShortDate(geboorteDatum);
			}
			else
			{
				foutmelding += "geen";
			}
		}

		return foutmelding;
	}

	private void verwerkNullBericht(String bsn, Client client, Vo107Bericht bericht)
	{

		GbaVraag laasteGbaVraag = gbaDao.getLaatsteGbaVraag(client, bsn);
		if (laasteGbaVraag != null)
		{
			laasteGbaVraag.setReactieOntvangen(true);
			hibernateService.saveOrUpdate(laasteGbaVraag);

			if (client != null)
			{
				client.setGbaStatus(GbaStatus.INDICATIE_AANGEVRAAGD);
			}

			GbaVraag gbaVraag = new GbaVraag();
			gbaVraag.setClient(client);
			gbaVraag.setBsn(bsn);
			gbaVraag.setDatum(currentDateSupplier.getDate());
			gbaVraag.setVraagType(GbaVraagType.PLAATS_INDICATIE);
			gbaVraag.setReactieOntvangen(false);
			gbaVraag.setAanvullendeInformatie(laasteGbaVraag.getAanvullendeInformatie());
			if (laasteGbaVraag.getClient() != null && GbaVraagType.VERWIJDER_INDICATIE.equals(laasteGbaVraag.getVraagType()))
			{
				gbaVraag.setReden(laasteGbaVraag.getReden());
			}

			hibernateService.saveOrUpdate(gbaVraag);
			if (client != null)
			{
				hibernateService.saveOrUpdate(client);
			}
		}
		else
		{
			LOG.error("Bericht niet verwerkt, onverwacht null bericht EREF: " + bericht.getString(Vo107_ArecordVeld.EREF));
		}
	}

	private void verwerkMutatie(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog, String bsn, Client client)
	{
		if (client != null)
		{

			if (bericht.getBerichtType().equals(WA11))
			{
				String anummer = bericht.getString(Vo107_ArecordVeld.ANR);
				Client otherClientWithSameAnummer = clientService.getClientByAnummer(anummer);
				if (otherClientWithSameAnummer != null)
				{
					String foutmelding = "Bericht(Wa11) geskipt: Client met bsn " + bsn + " en anummer " + anummer
						+ " kan niet gewijzigd worden, omdat er al een client met dit anummer (bsn " + otherClientWithSameAnummer.getPersoon().getBsn() + ") aanwezig is. "
						+ getFoutmelding(bericht, verwerkingLog, client);
					logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_SKIP, clientService.getScreeningOrganisatieVan(otherClientWithSameAnummer), otherClientWithSameAnummer,
						foutmelding);
					createFout(null, verwerkingLog, foutmelding, GbaFoutCategorie.OVERIG);
					return;
				}
			}
			GbaStatus oudeStatus = client.getGbaStatus();
			GbaVraag laasteGbaVraag = gbaDao.getLaatsteGbaVraag(client, bsn);
			verwerkBericht(bericht, verwerkingLog, client, laasteGbaVraag);
			aanvraagNieuweGegevensAfronden(client, laasteGbaVraag, oudeStatus);
		}
		else
		{
			GbaVraag gbaVraag = new GbaVraag();
			gbaVraag.setDatum(currentDateSupplier.getDate());
			gbaVraag.setBsn(bsn);
			gbaVraag.setVraagType(GbaVraagType.VERWIJDER_INDICATIE);
			gbaVraag.setReactieOntvangen(false);

			hibernateService.saveOrUpdate(gbaVraag);
		}
	}

	private boolean verstrekkingErrorClientMetAnderAnummerBestaatAl(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog, Client client)
	{
		String anummer = getStringUitBericht(bericht, GbaRubriek.PERS_A_NUMMER);
		Client otherClientWithSameAnummer = clientService.getClientByAnummer(anummer);
		if (otherClientWithSameAnummer != null)
		{
			String foutmelding = "Bericht geskipt: Client met bsn " + bericht.getBsn() + " en anummer " + anummer
				+ " kan niet aangemaakt worden, omdat er al een client met dit anummer (bsn " + otherClientWithSameAnummer.getPersoon().getBsn() + ") aanwezig is. "
				+ getFoutmelding(bericht, verwerkingLog, client);
			logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_SKIP, clientService.getScreeningOrganisatieVan(otherClientWithSameAnummer), otherClientWithSameAnummer,
				foutmelding);
			createFout(null, verwerkingLog, foutmelding, GbaFoutCategorie.OVERIG);
			return true;
		}
		return false;
	}

	private void verwerkVerstrekking(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog, Client client)
	{
		GbaVraag laasteGbaVraag = gbaDao.getLaatsteGbaVraag(client, bericht.getBsn());

		GbaStatus oudeStatus = null;
		if (client == null)
		{
			if (verstrekkingErrorClientMetAnderAnummerBestaatAl(bericht, verwerkingLog, client))
			{
				return;
			}
			verwerkingLog.setAantalNieuweBurgers(verwerkingLog.getAantalNieuweBurgers() + 1);

			client = vulNieuweClient(bericht, verwerkingLog);

			registreerMutatie(client, bericht);

			setIndicatieAanwezig(client);
			hibernateService.saveOrUpdate(client);
		}
		else
		{
			oudeStatus = client.getGbaStatus();
			verwerkBericht(bericht, verwerkingLog, client, laasteGbaVraag);
		}

		aanvraagNieuweGegevensAfronden(client, laasteGbaVraag, oudeStatus);
	}

	private Client vulNieuweClient(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog)
	{
		Client client = new Client();
		GbaPersoon persoon = new GbaPersoon();
		persoon.setPatient(client);
		client.setPersoon(persoon);
		vulPersoonsGegevens(client, bericht, verwerkingLog, true);
		verwerkAdres(bericht, verwerkingLog, client);

		var clientScreeningorganisatie = clientService.getScreeningOrganisatieVan(client);

		if (client.getPersoon().getGeboortedatum() == null)
		{
			logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_GEEN_GEBOORTEDATUM, clientScreeningorganisatie, null,
				"Verstrekking bevat geen geboortedatum, bsn: " + bericht.getBsn());
		}

		if (transgenderService.isNieuweClientAdhocPlaatsingTransgender(client.getPersoon()))
		{
			logService.logGebeurtenis(LogGebeurtenis.GBA_ADHOC_IMPORT_TRANSGENDER, clientScreeningorganisatie, client);
		}
		return client;
	}

	private void verwerkBericht(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog, Client client, GbaVraag laasteGbaVraag)
	{
		verwerkingLog.setAantalBijgewerkteBugers(verwerkingLog.getAantalBijgewerkteBugers() + 1);

		if (getScreeningOrganisatie(client) != null)
		{
			GbaVerwerkingEntry verwerkingEntry = getOrCreateEntry(verwerkingLog, client);
			verwerkingEntry.setAantalBijgewerkteBugers(verwerkingEntry.getAantalBijgewerkteBugers() + 1);
		}

		boolean gegevensGewijzigd = vulPersoonsGegevens(client, bericht, verwerkingLog, false);
		boolean adresGewijzigd = verwerkAdres(bericht, verwerkingLog, client);

		if (GbaStatus.INDICATIE_AANGEVRAAGD.equals(client.getGbaStatus()) && laasteGbaVraag != null && laasteGbaVraag.getClient() != null && laasteGbaVraag.getReden() != null)
		{
			boolean kanVersturenMetTijdelijkAdres = false;
			String aanvullendeInformatie = laasteGbaVraag.getAanvullendeInformatie();
			if (StringUtils.contains(aanvullendeInformatie, "|" + Constants.GBA_CHECK_ON_TIJDELIJK_ADRES_NU_ACTUEEL + "|"))
			{
				kanVersturenMetTijdelijkAdres = clientService.isTijdelijkeAdresNuActueel(client.getPersoon());
				if (adresGewijzigd)
				{
					aanvullendeInformatie = "|" + Constants.GBA_ADRES_GEGEVENS_GEWIJZIGD + aanvullendeInformatie;
				}
				client.getGbaMutaties().get(client.getGbaMutaties().size() - 1).setAanvullendeInformatie(aanvullendeInformatie);
			}
			RedenOpnieuwAanvragenClientgegevens reden = laasteGbaVraag.getReden();
			if (!gegevensGewijzigd && RedenOpnieuwAanvragenClientgegevens.ONJUISTE_PERSOONSGEGEVENS.equals(reden))
			{
				logService.logGebeurtenis(LogGebeurtenis.GBA_PERSOONSGEGEVENS_NIET_GEWIJZIGD, clientService.getScreeningOrganisatieVan(client), client);
			}
			else if (!adresGewijzigd && !kanVersturenMetTijdelijkAdres && RedenOpnieuwAanvragenClientgegevens.ONJUIST_ADRES.equals(reden)) 
			{
				logService.logGebeurtenis(LogGebeurtenis.GBA_ADRES_NIET_GEWIJZIGD, clientService.getScreeningOrganisatieVan(client), client);
			}
		}
		setIndicatieAanwezig(client);
		hibernateService.saveOrUpdate(client);
	}

	private void registreerMutatie(Client client, Vo107Bericht bericht)
	{
		GbaMutatie gbaMutatie = new GbaMutatie();
		gbaMutatie.setMutatieDatum(currentDateSupplier.getDate());
		client.setLaatsteGbaMutatie(gbaMutatie);
		gbaMutatie.setTypeBericht(bericht.getBerichtType());
		gbaMutatie.setBerichtEref(bericht.getString(Vo107_ArecordVeld.EREF));
		client.getGbaMutaties().add(gbaMutatie);
	}

	private void setIndicatieAanwezig(Client client)
	{
		if (!GbaStatus.PUNT_ADRES.equals(client.getGbaStatus()))
		{
			client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
		}
	}

	private void aanvraagNieuweGegevensAfronden(Client client, GbaVraag laasteGbaVraag, GbaStatus oudeStatus)
	{
		if (laasteGbaVraag != null && (isOpenstaandePlaatsing(laasteGbaVraag) || GbaStatus.INDICATIE_VERWIJDERD.equals(oudeStatus)))
		{
			laasteGbaVraag.setReactieOntvangen(true);
			hibernateService.saveOrUpdate(laasteGbaVraag);
			logService.logGebeurtenis(LogGebeurtenis.GBA_PERSOONSGEGEVENS_NIEUW_AANVRAGEN_AFGEROND, clientService.getScreeningOrganisatieVan(client), client);
		}
	}

	private boolean isOpenstaandePlaatsing(GbaVraag laasteGbaVraag)
	{
		return GbaVraagType.PLAATS_INDICATIE.equals(laasteGbaVraag.getVraagType()) && !Boolean.TRUE.equals(laasteGbaVraag.getReactieOntvangen());
	}

	private GbaVerwerkingEntry getOrCreateEntry(GbaVerwerkingsLog verwerkingLog, Client client)
	{
		GbaVerwerkingEntry verwerkingEntry = null;
		for (GbaVerwerkingEntry entry : verwerkingLog.getEntries())
		{
			if (entry.getScreeningOrganisatie().equals(getScreeningOrganisatie(client)))
			{
				verwerkingEntry = entry;
			}
		}

		if (verwerkingEntry == null)
		{
			verwerkingEntry = new GbaVerwerkingEntry();
			verwerkingEntry.setScreeningOrganisatie(getScreeningOrganisatie(client));
			verwerkingEntry.setVerwerkingsLog(verwerkingLog);
			verwerkingLog.getEntries().add(verwerkingEntry);
		}
		return verwerkingEntry;
	}

	private boolean verwerkAdres(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog, Client client)
	{

		GbaPersoon persoon = client.getPersoon();
		BagAdres adres = persoon.getGbaAdres();
		if (adres == null)
		{
			adres = new BagAdres();
		}

		boolean adresGegevensGewijzigd = vulAdresMetGbaGegevens(adres, bericht, client, verwerkingLog);
		adresGegevensGewijzigd |= changeProperty(adres, "postcodeCoordinaten", coordinatenDao.getCoordinaten(adres), true);
		persoon.setGbaAdres(adres);

		hibernateService.saveOrUpdate(adres);
		hibernateService.saveOrUpdate(persoon);

		boolean isTijdelijkGbaAdresVerwijderd = false;

		if (".".equals(StringUtils.trim(adres.getStraat())) && StringUtils.isNotBlank(getStringUitBericht(bericht, GbaRubriek.VERBP_AAND_GEG_ONDERZOEK)))
		{
			client.setGbaStatus(GbaStatus.PUNT_ADRES);
		}
		else
		{
			if (GbaStatus.PUNT_ADRES.equals(client.getGbaStatus()))
			{
				client.setGbaStatus(null);
			}
			if (AdresUtil.isOnvolledigAdres(adres))
			{
				if (adres.getGbaGemeente() != null && !adres.getGbaGemeente().getCode().equals(Gemeente.RNI_CODE) && client.getPersoon().getDatumVertrokkenUitNederland() == null)
				{
					String melding = "Lege postcode (08.11.60) en/of lege huisnummer (08.11.20) en/of lege woonplaats (08.11.70) en/of gevulde locatieomschrijving (08.12.10) "
						+ getFoutmelding(bericht, verwerkingLog, client);
					logService.logGebeurtenis(LogGebeurtenis.GBA_ADRES_ONVOLLEDIG, clientService.getScreeningOrganisatieVan(client), client, melding);
				}
			}
			else
			{
				TijdelijkGbaAdres tijdelijkGbaAdres = persoon.getTijdelijkGbaAdres();
				if (tijdelijkGbaAdres != null)
				{
					if (tijdelijkGbaAdres.getId() != null)
					{
						isTijdelijkGbaAdresVerwijderd = true;
						String melding = "Automatisch verwijderd. " + getFoutmelding(bericht, verwerkingLog, client);
						logService.logGebeurtenis(LogGebeurtenis.GBA_TIJDELIJK_ADRES, clientService.getScreeningOrganisatieVan(client), client, melding);
						hibernateService.delete(tijdelijkGbaAdres);
					}
					persoon.setTijdelijkGbaAdres(null);
					hibernateService.saveOrUpdate(persoon);
				}
			}
			setGbaAdresGewijzigdMarker(client, adresGegevensGewijzigd, isTijdelijkGbaAdresVerwijderd);
		}
		return adresGegevensGewijzigd;
	}

	private void setGbaAdresGewijzigdMarker(Client client, boolean adresGewijzigd, boolean tijdelijkGbaVerwijderd)
	{

		GbaMutatie huidigeMutatie = getHuidigeGbaMutatie(client);
		if (huidigeMutatie != null && client.getMammaDossier() != null
			&& (client.getMammaDossier().getLaatsteScreeningRonde() != null || client.getMammaDossier().getLaatsteAfmelding() != null)
			&& (adresGewijzigd || tijdelijkGbaVerwijderd))
		{
			huidigeMutatie.setAanvullendeInformatie(Constants.MAMMA_ADRES_GEWIJZIGD_MARKER + huidigeMutatie.getAanvullendeInformatie());
		}
	}

	private void plaatsBSNGewijzigdMarker(Client client, String oorspronkelijkBsn, String nieuweBsn)
	{
		GbaMutatie huidigeMutatie = getHuidigeGbaMutatie(client);

		if (huidigeMutatie != null && client.getMammaDossier() != null
			&& (client.getMammaDossier().getLaatsteScreeningRonde() != null || client.getMammaDossier().getLaatsteAfmelding() != null)
			&& !StringUtils.contains(huidigeMutatie.getAanvullendeInformatie(), Constants.MAMMA_IMS_CLIENT_BSN_GEWIJZIGD_MARKER))
		{
			huidigeMutatie.setAanvullendeInformatie(
				String.format("|%s:%s,%s|%s", Constants.MAMMA_IMS_CLIENT_BSN_GEWIJZIGD_MARKER, oorspronkelijkBsn, nieuweBsn, huidigeMutatie.getAanvullendeInformatie()));
		}
	}

	private GbaMutatie getHuidigeGbaMutatie(Client client)
	{
		if (client == null)
		{
			return null;
		}
		return !client.getGbaMutaties().isEmpty() ? client.getGbaMutaties().get(client.getGbaMutaties().size() - 1) : null;
	}

	private void plaatsIMSGegevensGewijzigdMarker(Client client)
	{

		plaatsMammaMarker(client, Constants.MAMMA_IMS_CLIENT_GEGEVENS_GEWIJZIGD_MARKER);
	}

	private void plaatsMammaMarker(Client client, String marker)
	{
		GbaMutatie huidigeMutatie = getHuidigeGbaMutatie(client);
		if (huidigeMutatie != null && client.getMammaDossier() != null && client.getMammaDossier().getLaatsteScreeningRonde() != null
			&& !StringUtils.contains(huidigeMutatie.getAanvullendeInformatie(), marker))
		{
			huidigeMutatie.setAanvullendeInformatie(marker + huidigeMutatie.getAanvullendeInformatie());
		}
	}

	private void verwerkTabelRegel(Vo107Bericht bericht)
	{
		if (bericht.getRubriekMap().containsKey(GbaRubriek.LAND_CODE.getNummer()))
		{

			String landCode = getStringUitBericht(bericht, GbaRubriek.LAND_CODE);
			Land land = gbaDao.getStamtabelByCode(Land.class, landCode);
			if (land == null)
			{
				land = new Land();
				land.setCode(landCode);
			}

			String naam = getStringUitBericht(bericht, GbaRubriek.LAND_NAAM);
			Date beginDatum = getDateUitBericht(bericht, GbaRubriek.LAND_BEGINDATUM);
			Date eindDatum = getDateUitBericht(bericht, GbaRubriek.LAND_EINDDATUM);

			if (naam != null)
			{
				land.setNaam(naam);
			}

			if (beginDatum != null)
			{
				land.setBeginDatum(beginDatum);
			}

			if (eindDatum != null)
			{
				land.setEindDatum(eindDatum);
			}

			hibernateService.saveOrUpdate(land);
		}
		else if (bericht.getRubriekMap().containsKey(GbaRubriek.NATIONALITEIT_CODE.getNummer()))
		{

			String nationaliteitCode = getStringUitBericht(bericht, GbaRubriek.NATIONALITEIT_CODE);
			Nationaliteit nationaliteit = gbaDao.getStamtabelByCode(Nationaliteit.class, nationaliteitCode);
			if (nationaliteit == null)
			{
				nationaliteit = new Nationaliteit();
				nationaliteit.setCode(nationaliteitCode);
			}

			String naam = getStringUitBericht(bericht, GbaRubriek.NATIONALTIEIT_NAAM);
			Date beginDatum = getDateUitBericht(bericht, GbaRubriek.NATIONALITEIT_BEGINDATUM);
			Date eindDatum = getDateUitBericht(bericht, GbaRubriek.NATIONALITEIT_EINDDATUM);

			if (naam != null)
			{
				nationaliteit.setNaam(naam);
			}

			if (beginDatum != null)
			{
				nationaliteit.setBeginDatum(beginDatum);
			}

			if (eindDatum != null)
			{
				nationaliteit.setEindDatum(eindDatum);
			}

			hibernateService.saveOrUpdate(nationaliteit);
		}
		else if (bericht.getRubriekMap().containsKey(GbaRubriek.TITEL_CODE.getNummer()))
		{
			String titelCode = getStringUitBericht(bericht, GbaRubriek.TITEL_CODE);
			List<Client> clienten = clientService.getClientenMetTitel(titelCode);

			for (Client client : clienten)
			{
				client.getPersoon().setTitel(getStringUitBericht(bericht, GbaRubriek.TITEL_OMSCHRIJVING));
			}

			hibernateService.saveOrUpdateAll(clienten);
		}
		else if (bericht.getRubriekMap().containsKey(GbaRubriek.GEMEENTE_CODE.getNummer()))
		{
			String gemeenteCode = getStringUitBericht(bericht, GbaRubriek.GEMEENTE_CODE);
			Gemeente gemeente = gbaDao.getStamtabelByCode(Gemeente.class, gemeenteCode);

			if (gemeente == null)
			{
				gemeente = new Gemeente();
				gemeente.setCode(gemeenteCode);
			}

			String naam = getStringUitBericht(bericht, GbaRubriek.GEMEENTE_NAAM);
			Date beginDatum = getDateUitBericht(bericht, GbaRubriek.GEMEENTE_BEGINDATUM);
			Date eindDatum = getDateUitBericht(bericht, GbaRubriek.GEMEENTE_EINDDATUM);

			String nieuweGemeenteCode = getStringUitBericht(bericht, GbaRubriek.NIEUWE_GEMEENTE_CODE);

			if (naam != null)
			{
				gemeente.setNaam(naam);
			}

			if (beginDatum != null)
			{
				gemeente.setBeginDatum(beginDatum);
			}

			if (eindDatum != null)
			{
				gemeente.setEindDatum(eindDatum);
			}

			if (nieuweGemeenteCode != null)
			{
				Gemeente nieuweGemeente = gbaDao.getStamtabelByCode(Gemeente.class, nieuweGemeenteCode);
				gemeente.setOpvolgGemeente(nieuweGemeente);
			}

			hibernateService.saveOrUpdate(gemeente);
		}
	}

	private boolean vulAdresMetGbaGegevens(BagAdres adres, Vo107Bericht bericht, Client client, GbaVerwerkingsLog verwerkingsLog)
	{

		String huisnummerString = getStringUitBericht(bericht, GbaRubriek.VERBP_HUISNR);
		String postcode = getStringUitBericht(bericht, GbaRubriek.VERBP_POSTCODE);
		String huisletter = getStringUitBericht(bericht, GbaRubriek.VERBP_HUISLETTER);
		String toevoeging = getStringUitBericht(bericht, GbaRubriek.VERBP_HUISNRTOEV);
		String aanduiding = getStringUitBericht(bericht, GbaRubriek.VERBP_AAND_HUISNR);
		String straat = getStringUitBericht(bericht, GbaRubriek.VERBP_STRAATNAAM);
		String gemeenteDeel = getStringUitBericht(bericht, GbaRubriek.VERBP_GEMEENTEDEEL);
		String woonplaats = getStringUitBericht(bericht, GbaRubriek.VERBP_WOONPLAATS);
		String locatieBeschrijving = getStringUitBericht(bericht, GbaRubriek.VERBP_LOC_BESCHR);
		String gemeenteCode = getCodeUitBericht(bericht, GbaRubriek.VERBP_GEMEENTE_INSCHR);
		String naamOpenbareRuimte = getStringUitBericht(bericht, GbaRubriek.VERBP_NAAMOPENBARERUIMTE);
		String identificatieCodeVerblijfplaats = getStringUitBericht(bericht, GbaRubriek.VERBP_IDVERBLIJFPLAATS);
		String identificatieCodeNummerAanduiding = getStringUitBericht(bericht, GbaRubriek.VERBP_IDNUMMERAANDUIDING);

		boolean verstrekking = bericht.isVerstrekking();

		adres.setLand(nl.topicuszorg.gba.model.Land.NEDERLAND);

		Integer huisnummer = null;
		if (!Strings.isNullOrEmpty(huisnummerString))
		{
			huisnummer = Integer.valueOf(huisnummerString);
		}

		boolean adresGegevensGewijzigd = changeProperty(adres, "huisnummer", huisnummer, verstrekking || huisnummerString != null);
		adresGegevensGewijzigd |= changeProperty(adres, "postcode", PostcodeFormatter.formatPostcode(postcode, false), verstrekking || postcode != null);

		if (StringUtils.isBlank(huisletter))
		{
			adresGegevensGewijzigd |= changeProperty(adres, "huisletter", huisletter, verstrekking);
		}
		else
		{
			adresGegevensGewijzigd |= changeProperty(adres, "huisletter", huisletter.substring(0, 1), true);
		}

		adresGegevensGewijzigd |= changeProperty(adres, "huisnummerToevoeging", toevoeging, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "huisnummerAanduiding", aanduiding, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "straat", straat, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "gemeentedeel", gemeenteDeel, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "plaats", woonplaats, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "locatieBeschrijving", locatieBeschrijving, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "naamOpenbareRuimte", naamOpenbareRuimte, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "identificatieCodeVerblijfplaats", identificatieCodeVerblijfplaats, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "identificatieCodeNummerAanduiding", identificatieCodeNummerAanduiding, verstrekking);

		Gemeente gemeente = null;
		if (gemeenteCode != null)
		{
			gemeente = getOrCreateGemeente(bericht, client, verwerkingsLog, gemeenteCode);
		}
		adresGegevensGewijzigd |= changeProperty(adres, "gbaGemeente", gemeente, verstrekking || gemeenteCode != null);
		return adresGegevensGewijzigd;
	}

	private Gemeente getOrCreateGemeente(Vo107Bericht bericht, Client client, GbaVerwerkingsLog verwerkingsLog, String gemeenteCode)
	{
		Gemeente gemeente = gbaDao.getStamtabelByCode(Gemeente.class, gemeenteCode);

		if (gemeente == null)
		{
			gemeente = new Gemeente();
			gemeente.setCode(gemeenteCode);
			gemeente.setNaam(getStringUitBericht(bericht, GbaRubriek.VERBP_GEMEENTE_INSCHR));

			hibernateService.saveOrUpdate(client);

			createFout(client, verwerkingsLog, "Gemeente was niet bekend, toegevoegd. Gemeentecode: " + gemeenteCode, GbaFoutCategorie.INHOUDELIJK);
			logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_GEMEENTE_TOEGEVOEGD, null, "Gemeente was niet bekend, toegevoegd. Gemeentecode: " + gemeenteCode);
			hibernateService.saveOrUpdate(gemeente);
		}
		return gemeente;
	}

	private boolean vulPersoonsGegevens(Client client, Vo107Bericht bericht, GbaVerwerkingsLog verwerkingsLog, boolean isNieuw)
	{
		boolean persoonsGegevensGewijzigd = false;

		String bsn = getStringUitBericht(bericht, GbaRubriek.PERS_BSN);
		String anummer = getStringUitBericht(bericht, GbaRubriek.PERS_A_NUMMER);
		String arecordAnummer = bericht.getString(Vo107_ArecordVeld.ANR);
		String tussenvoegselGeslachtsnaam = getStringUitBericht(bericht, GbaRubriek.PERS_VOORV_GESLACHTSNAAM);
		String geslachtsnaam = getStringUitBericht(bericht, GbaRubriek.PERS_GESLACHTSNAAM);
		String voornaam = getStringUitBericht(bericht, GbaRubriek.PERS_VOORNAMEN_01_02_10);
		NaamGebruik naamgebruik = getNaamGebruikUitBericht(bericht, GbaRubriek.PERS_NAAMGEBRUIK);
		String adelijkeTitel = getStringUitBericht(bericht, GbaRubriek.PERS_TITELPREDIKAAT);
		String codeTitel = getCodeUitBericht(bericht, GbaRubriek.PERS_TITELPREDIKAAT);

		Date geboorteDatum = getDateUitBericht(bericht, GbaRubriek.PERS_GEBOORTEDATUM);
		DatumPrecisie geboorteDatumPrecisie = getDatumPrecisieUitBericht(bericht, GbaRubriek.PERS_GEBOORTEDATUM);
		Date overlijdensDatum = getDateUitBericht(bericht, GbaRubriek.OVL_DATUM_OVERLIJDEN);

		Geslacht geslacht = getGeslachtUitBericht(bericht, GbaRubriek.PERS_GESLACHTSAANDUIDING);

		Date datumVertrekUitNederland = getDateUitBericht(bericht, GbaRubriek.VERBP_DATUM_VERTREK_NED);
		Date datumVestigingInNederland = getDateUitBericht(bericht, GbaRubriek.VERBP_DATUM_VESTIGING_NED);
		String geheim = getStringUitBericht(bericht, GbaRubriek.INSCH_INDICATIE_GEHEIM);
		Date datumAanvangAdreshouding = getDateUitBericht(bericht, GbaRubriek.VERBP_DATUM_AANV_ADRESH);

		String registerGemeenteAkte = getCodeUitBericht(bericht, GbaRubriek.OVL_REGISTERGEMEENTE_AKTE);
		String akteNummerOverlijden = getStringUitBericht(bericht, GbaRubriek.OVL_AKTENUMMER);

		boolean verstrekking = bericht.isVerstrekking();

		GbaPersoon persoon = client.getPersoon();

		if (!Strings.isNullOrEmpty(bsn))
		{
			String oorspronkelijkBsn = persoon.getBsn();
			persoonsGegevensGewijzigd |= changeProperty(persoon, "bsn", bsn, false);
			persoon.setBsnGeverifieerd(Boolean.TRUE);
			if (persoonsGegevensGewijzigd)
			{
				plaatsBSNGewijzigdMarker(client, oorspronkelijkBsn, bsn);
			}
		}

		if (!Strings.isNullOrEmpty(arecordAnummer) && bericht.getBerichtType().equals(WA11))
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "anummer", arecordAnummer, false);
		}
		else if (!Strings.isNullOrEmpty(anummer))
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "anummer", anummer, false);
		}

		boolean isAchternaamGewijzigd = changeProperty(persoon, "achternaam", geslachtsnaam, verstrekking);
		persoonsGegevensGewijzigd |= isAchternaamGewijzigd;
		boolean imsGegevensGewijzigd = isAchternaamGewijzigd;

		if (Strings.isNullOrEmpty(geslachtsnaam) && isNieuw)
		{
			persoon.setAchternaam("");

			hibernateService.saveOrUpdate(client);
			createFout(client, verwerkingsLog, "Verstrekking bevat geen geslachtsnaam, bsn: " + bsn, GbaFoutCategorie.INHOUDELIJK_ERNGSTIG);
		}

		boolean isTussenvoegselGewijzigd = changeProperty(persoon, "tussenvoegsel", tussenvoegselGeslachtsnaam, verstrekking);
		persoonsGegevensGewijzigd |= isTussenvoegselGewijzigd;
		imsGegevensGewijzigd |= isTussenvoegselGewijzigd;

		boolean isVoornaamGewijzigd = changeProperty(persoon, "voornaam", voornaam, verstrekking);
		persoonsGegevensGewijzigd |= isVoornaamGewijzigd;
		imsGegevensGewijzigd |= isVoornaamGewijzigd;

		persoonsGegevensGewijzigd |= changeProperty(persoon, "naamGebruik", naamgebruik, verstrekking);

		if (adelijkeTitel != null)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "titel", adelijkeTitel, verstrekking);
			persoonsGegevensGewijzigd |= changeProperty(persoon, "titelCode", codeTitel, verstrekking || codeTitel != null);
		}
		else if (verstrekking)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "titel", null, true);
			persoonsGegevensGewijzigd |= changeProperty(persoon, "titelCode", null, true);
		}

		if (geboorteDatum != null)
		{
			boolean isGeboortedatumGewijzigd = changeProperty(persoon, "geboortedatum", geboorteDatum, verstrekking);
			persoonsGegevensGewijzigd |= isGeboortedatumGewijzigd;
			imsGegevensGewijzigd |= isGeboortedatumGewijzigd;

			persoonsGegevensGewijzigd |= changeProperty(persoon, "geboortedatumPrecisie", geboorteDatumPrecisie, verstrekking);
		}
		else if (isNieuw)
		{

			hibernateService.saveOrUpdate(client);
			createFout(client, verwerkingsLog, "Verstrekking bevat geen geboortedatum, bsn: " + bsn, GbaFoutCategorie.INHOUDELIJK_ERNGSTIG);
		}
		else if (verstrekking)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "geboortedatum", null, true);
			persoonsGegevensGewijzigd |= changeProperty(persoon, "geboortedatumPrecisie", null, true);
		}

		if (overlijdensDatum != null)
		{
			boolean isOverlijdensdatumGewijzigd = changeProperty(persoon, "overlijdensdatum", overlijdensDatum, verstrekking);
			persoonsGegevensGewijzigd |= isOverlijdensdatumGewijzigd;
			imsGegevensGewijzigd |= isOverlijdensdatumGewijzigd;
		}
		else if (getStringUitBericht(bericht, GbaRubriek.OVL_DATUM_OVERLIJDEN) != null || verstrekking)
		{
			boolean isOverlijdingsdatumGewijzigd = changeProperty(persoon, "overlijdensdatum", null, true);
			persoonsGegevensGewijzigd |= isOverlijdingsdatumGewijzigd;
			imsGegevensGewijzigd |= isOverlijdingsdatumGewijzigd;
		}

		boolean isGeslachtGewijzigd = changeProperty(persoon, "geslacht", geslacht, verstrekking || getStringUitBericht(bericht, GbaRubriek.PERS_GESLACHTSAANDUIDING) != null);
		persoonsGegevensGewijzigd |= isGeslachtGewijzigd;
		imsGegevensGewijzigd |= isGeslachtGewijzigd;

		if (geheim != null)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "indicatieGeheim", IndicatieGeheim.getByCode(geheim), verstrekking);
		}
		else if (verstrekking)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "indicatieGeheim", null, true);
		}

		if (StringUtils.isBlank(registerGemeenteAkte))
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "registerGemeenteAkteOverlijden", null, true);
		}
		else
		{
			Gemeente gemeente = getOrCreateGemeente(bericht, client, verwerkingsLog, registerGemeenteAkte);
			persoonsGegevensGewijzigd |= changeProperty(persoon, "registerGemeenteAkteOverlijden", gemeente, verstrekking);
		}

		persoonsGegevensGewijzigd |= changeProperty(persoon, "akteNummerOverlijden", akteNummerOverlijden, verstrekking);

		if (datumAanvangAdreshouding != null)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumAanvangAdreshouding", datumAanvangAdreshouding, verstrekking);
		}
		else if (getStringUitBericht(bericht, GbaRubriek.VERBP_DATUM_AANV_ADRESH) != null || verstrekking)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumAanvangAdreshouding", null, true);
		}

		if (datumVestigingInNederland != null)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumVestigingNederland", datumVestigingInNederland, verstrekking);
		}
		else if (getStringUitBericht(bericht, GbaRubriek.VERBP_DATUM_VESTIGING_NED) != null || verstrekking)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumVestigingNederland", null, true);
		}

		if (datumVertrekUitNederland != null)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumVertrokkenUitNederland", datumVertrekUitNederland, verstrekking);
		}
		else if (getStringUitBericht(bericht, GbaRubriek.VERBP_DATUM_VERTREK_NED) != null || verstrekking)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumVertrokkenUitNederland", null, true);
		}

		GbaPartnerschap partnerschap = bericht.getMeestRecentPartnerschap();
		if (partnerschap != null)
		{

			if (verstrekking || partnerschap.isGeslachtsnaamPartnerInBericht())
			{
				persoonsGegevensGewijzigd |= changeProperty(persoon, "partnerAchternaam", partnerschap.getGeslachtsnaamPartner(), true);
				persoonsGegevensGewijzigd |= changeProperty(persoon, "partnerTussenvoegsel", partnerschap.getVoorvoegselPartner(), true);
				persoonsGegevensGewijzigd |= changeProperty(persoon, "datumAangaanPartnerschap", partnerschap.getPartnerschapSluiting(), true);
				persoonsGegevensGewijzigd |= changeProperty(persoon, "datumOntbindingPartnerschap", partnerschap.getPartnerschapEinde(), true);
			}
			else if (partnerschap.isPartnerschapEindeInBericht())
			{
				if (partnerschap.isGeslachtsnaamPartnerInBericht())
				{
					persoonsGegevensGewijzigd |= changeProperty(persoon, "partnerAchternaam", partnerschap.getGeslachtsnaamPartner(), true);
				}

				if (partnerschap.isVoorvoegselPartnerInBericht())
				{
					persoonsGegevensGewijzigd |= changeProperty(persoon, "partnerTussenvoegsel", partnerschap.getVoorvoegselPartner(), true);
				}

				if (partnerschap.isPartnerschapSluitingInBericht())
				{
					persoonsGegevensGewijzigd |= changeProperty(persoon, "datumAangaanPartnerschap", partnerschap.getPartnerschapSluiting(), true);
				}

				persoonsGegevensGewijzigd |= changeProperty(persoon, "datumOntbindingPartnerschap", partnerschap.getPartnerschapEinde(), true);
			}
		}
		else if (verstrekking)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "partnerAchternaam", null, true);
			persoonsGegevensGewijzigd |= changeProperty(persoon, "partnerTussenvoegsel", null, true);
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumAangaanPartnerschap", null, true);
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumOntbindingPartnerschap", null, true);
		}

		if (imsGegevensGewijzigd)
		{
			plaatsIMSGegevensGewijzigdMarker(client);
		}

		return persoonsGegevensGewijzigd;
	}

	private boolean changeProperty(Object target, String property, Object newValue, boolean blankOnNull)
	{
		boolean propertyChanged = false;
		if (newValue != null || blankOnNull)
		{
			try
			{
				if (newValue != null && newValue.toString().trim().equals(""))
				{
					newValue = null;
				}
				Object oldValue = PropertyUtils.getProperty(target, property);
				propertyChanged = oldValue == null && newValue != null || oldValue != null && !oldValue.equals(newValue);

				if (propertyChanged)
				{

					if (newValue == null && oldValue != null && oldValue.toString().trim().equals(""))
					{
						propertyChanged = false;
					}
					if (LOG.isTraceEnabled())
					{
						LOG.trace("value of " + property + " is gewijzigd van " + oldValue + " to " + newValue);
					}
					PropertyUtils.setProperty(target, property, newValue);
				}
			}
			catch (SecurityException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
			{
				LOG.error("changeProperty fail: {}", e.toString());
			}
		}
		return propertyChanged;
	}

	public void createFout(Client client, GbaVerwerkingsLog verwerkingsLog, String fout, GbaFoutCategorie foutcat)
	{
		GbaFoutRegel gbaFoutRegel = new GbaFoutRegel();
		if (client != null)
		{
			gbaFoutRegel.setClient(client.getId());
		}
		gbaFoutRegel.setFout(fout);
		gbaFoutRegel.setFoutCategorie(foutcat);
		gbaFoutRegel.setVerwerkingsLog(verwerkingsLog);
		LOG.error("GbaFoutRegel aangemaakt (cat:" + foutcat + ") " + (client != null ? "voor client met id " + client.getId() : ""));
		verwerkingsLog.getFouten().add(gbaFoutRegel);
	}

	private NaamGebruik getNaamGebruikUitBericht(Vo107Bericht bericht, GbaRubriek gbaRubriek)
	{
		String stringValue = getStringUitBericht(bericht, gbaRubriek);

		if (!Strings.isNullOrEmpty(stringValue))
		{
			stringValue = stringValue.trim();
			return NaamGebruik.getNaamGebruikByGba(stringValue);
		}

		return null;
	}

	private String getStringUitBericht(Vo107Bericht bericht, GbaRubriek gbaRubriek)
	{
		Record<VoxBrecordVeld> record = bericht.getSingleRubriek(gbaRubriek.getNummer());
		if (record == null)
		{
			return null;
		}
		return record.getWaarde(VoxBrecordVeld.INH);
	}

	private String getCodeUitBericht(Vo107Bericht bericht, GbaRubriek gbaRubriek)
	{
		Record<VoxBrecordVeld> record = bericht.getSingleRubriek(gbaRubriek.getNummer());
		if (record == null)
		{
			return null;
		}
		return record.getWaarde(VoxBrecordVeld.CODE);
	}

	private Date getDateUitBericht(Vo107Bericht bericht, GbaRubriek gbaRubriek)
	{
		String stringValue = getStringUitBericht(bericht, gbaRubriek);
		if (stringValue == null || stringValue.startsWith("0000"))
		{
			return null;
		}
		return VoxHelper.convertToDate(stringValue);
	}

	private DatumPrecisie getDatumPrecisieUitBericht(Vo107Bericht bericht, GbaRubriek gbaRubriek)
	{
		String stringValue = getStringUitBericht(bericht, gbaRubriek);

		if (stringValue != null && stringValue.endsWith("0000"))
		{
			return DatumPrecisie.JAAR;
		}
		else if (stringValue != null && stringValue.endsWith("00"))
		{
			return DatumPrecisie.MAAND;
		}

		return DatumPrecisie.VOLLEDIG;
	}

	private Geslacht getGeslachtUitBericht(Vo107Bericht bericht, GbaRubriek gbaRubriek)
	{
		String stringValue = getStringUitBericht(bericht, gbaRubriek);

		if (stringValue == null)
		{
			return null;
		}

		if (stringValue.startsWith("O"))
		{
			return Geslacht.ONBEKEND;
		}
		else if (stringValue.startsWith("M"))
		{
			return Geslacht.MAN;
		}
		else if (stringValue.startsWith("V"))
		{
			return Geslacht.VROUW;
		}
		return null;
	}

	private Long getScreeningOrganisatie(Client client)
	{
		if (client == null || client.getPersoon() == null || client.getPersoon().getGbaAdres() == null || client.getPersoon().getGbaAdres().getGbaGemeente() == null
			|| client.getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie() == null)
		{
			return null;
		}

		return client.getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie().getId();
	}

	@Override
	public Criteria getAllAdressenZonderCoordinanten()
	{
		return gbaDao.getAllAdressenZonderCoordinanten();
	}
}
