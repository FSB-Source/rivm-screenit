package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.batch.exceptions.OngeldigCdaException;
import nl.rivm.screenit.batch.service.VerwerkCdaBerichtContentService;
import nl.rivm.screenit.batch.service.VerwerkCdaBerichtService;
import nl.rivm.screenit.dao.CdaVerslagDao;
import nl.rivm.screenit.dao.KwaliteitsovereenkomstDao;
import nl.rivm.screenit.dao.cervix.CervixBMHKLaboratoriumDao;
import nl.rivm.screenit.hl7v3.cda.ClinicalDocument;
import nl.rivm.screenit.hl7v3.cda.EnFamily;
import nl.rivm.screenit.hl7v3.cda.EnGiven;
import nl.rivm.screenit.hl7v3.cda.EnPrefix;
import nl.rivm.screenit.hl7v3.cda.II;
import nl.rivm.screenit.hl7v3.cda.PN;
import nl.rivm.screenit.hl7v3.cda.POCDMT000040AssignedAuthor;
import nl.rivm.screenit.hl7v3.cda.POCDMT000040AssignedEntity;
import nl.rivm.screenit.hl7v3.cda.POCDMT000040Organization;
import nl.rivm.screenit.hl7v3.cda.POCDMT000040Person;
import nl.rivm.screenit.hl7v3.cda.helper.CDAHelper;
import nl.rivm.screenit.hl7v3.cda.helper.CommonCdaConstants;
import nl.rivm.screenit.hl7v3.cda.helper.ExtractCDA;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.CdaConstants;
import nl.rivm.screenit.model.berichten.cda.CdaOID;
import nl.rivm.screenit.model.berichten.cda.MeldingOngeldigCdaBericht;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.cervix.CervixCytologieOrder;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieVerslagContent;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaLaboratorium;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.colon.verslag.pa.PaVerslagContent;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.BerichtOntvangenLogEvent;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.verslag.MammaVerslag;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpVerslagContent;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.GebruikersService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.VerwerkVerslagService;
import nl.rivm.screenit.util.MedewerkerUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class VerwerkCdaBerichtServiceImpl implements VerwerkCdaBerichtService
{

	private static final Logger LOG = LoggerFactory.getLogger(VerwerkCdaBerichtServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CdaVerslagDao cdaVerslagDao;

	@Autowired
	private ClientService clientService;

	@Autowired
	private InstellingService instellingService;

	@Autowired
	private GebruikersService gebruikersService;

	@Autowired
	private LogService logService;

	@Autowired
	private VerwerkVerslagService verwerkVerslagService;

	@Autowired
	private VerwerkCdaBerichtContentService verwerkCdaBerichtContentService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private KwaliteitsovereenkomstDao kwaliteitsovereenkomstDao;

	@Autowired(required = false)
	private CervixBMHKLaboratoriumDao bmhkLaboratoriumDao;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwerkBericht(Long berichtID) throws Exception
	{

		LOG.info("Bericht " + berichtID + ": verwerking gestart.");
		OntvangenCdaBericht ontvangenCdaBericht = hibernateService.load(OntvangenCdaBericht.class, berichtID);
		ClinicalDocument cdaDocument = ExtractCDA.getCDADocument(ontvangenCdaBericht.getXmlBericht());
		BerichtType berichtType = ontvangenCdaBericht.getBerichtType();
		if (BerichtStatus.VERWERKT.equals(ontvangenCdaBericht.getStatus()))
		{
			LOG.info("Verwerkt bericht " + berichtID + " opnieuw verwerken.");
			if (BerichtType.CERVIX_CYTOLOGIE_VERSLAG.equals(berichtType))
			{
				LOG.warn("Bericht " + berichtID + " nogmaals verwerken niet mogelijk. Geskipped.");
			}
			else
			{
				Verslag bestaandeVerslag = getVerslagVoorSetID(ontvangenCdaBericht, cdaDocument);
				if (bestaandeVerslag != null)
				{
					switch (berichtType)
					{
					case MDL_VERSLAG:
						MdlVerslag mdlVerslag = (MdlVerslag) bestaandeVerslag;
						verwerkVerslagService.ontkoppelOfVerwijderComplicaties(mdlVerslag);
						verwerkMdlVerslagContent(cdaDocument, mdlVerslag);
						break;
					case PA_LAB_VERSLAG:
						verwerkPaLabVerslagContent((PaVerslag) bestaandeVerslag);
						break;
					case CERVIX_CYTOLOGIE_VERSLAG:

						break;
					case MAMMA_PA_FOLLOW_UP_VERSLAG:
						verwerkMammaFollowUpPaVerslagContent((MammaFollowUpVerslag) bestaandeVerslag);
						break;
					}
					bestaandeVerslag.setDatumVerwerkt(currentDateSupplier.getDate());
					hibernateService.saveOrUpdate(bestaandeVerslag);
					verwerkVerslagService.verwerkInDossier(bestaandeVerslag);
					verwerkVerslagService.onAfterVerwerkVerslagContent(bestaandeVerslag);
					LOG.warn("Bericht " + berichtID + " opnieuw verwerkt.");
				}
				else
				{
					LOG.warn("Bericht " + berichtID + " bestond niet eerder. Geskipped.");
				}
			}
		}
		else
		{
			List<Instelling> instellingen = new ArrayList<>();
			instellingen.add(hibernateService.loadAll(Rivm.class).get(0));
			try
			{
				Verslag verslag = null;
				switch (berichtType)
				{
				case MDL_VERSLAG:
					verslag = new MdlVerslag();
					break;
				case PA_LAB_VERSLAG:
					verslag = new PaVerslag();
					break;
				case CERVIX_CYTOLOGIE_VERSLAG:
					verslag = new CervixCytologieVerslag();
					break;
				case MAMMA_PA_FOLLOW_UP_VERSLAG:
					verslag = new MammaFollowUpVerslag();
					break;
				}
				verslag.setType(berichtType.getVerslagType());
				verslag.setOntvangenBericht(ontvangenCdaBericht);
				verslag.setDatumVerwerkt(currentDateSupplier.getDate());
				verslag.setStatus(VerslagStatus.AFGEROND);

				verwerkUitvoerderInVerslag(verslag, cdaDocument);
				verslag = verwerkBvoSpecifiekeGegevens(verslag, cdaDocument);

				saveOrReplaceVerslag(verslag, cdaDocument);
				ontvangenCdaBericht.setStatus(BerichtStatus.VERWERKT);
				hibernateService.saveOrUpdate(ontvangenCdaBericht);
				verwerkVerslagService.verwerkInDossier(verslag);
				verwerkVerslagService.onAfterVerwerkVerslagContent(verslag);

				Instelling uitvoerendeInstelling = verslag.getUitvoerderOrganisatie();
				String melding = "CDA met berichtId " + ontvangenCdaBericht.getBerichtId() + ", setId " + ontvangenCdaBericht.getSetId() + " en versie "
					+ ontvangenCdaBericht.getVersie() + " is verwerkt. Uitvoerder: "
					+ (uitvoerendeInstelling != null ? uitvoerendeInstelling.getNaam() : ((MammaVerslag) verslag).getLabCode());
				BerichtOntvangenLogEvent logEvent = new BerichtOntvangenLogEvent();
				logEvent.setBericht(ontvangenCdaBericht);
				logEvent.setMelding(melding);

				if (uitvoerendeInstelling != null)
				{
					instellingen.add(uitvoerendeInstelling);
				}
				logService.logGebeurtenis(berichtType.getLbBerichtVerwerkt(), instellingen, logEvent, null, verslag.getScreeningRonde().getDossier().getClient(),
					verslag.getType().getBevolkingsonderzoek());
				LOG.info("Bericht " + berichtID + ": verwerkt.");
			}
			catch (OngeldigCdaException e)
			{
				String melding = "CDA met berichtId " + ontvangenCdaBericht.getBerichtId() + ", setId " + ontvangenCdaBericht.getSetId() + " en versie "
					+ ontvangenCdaBericht.getVersie() + " moet handmatig verwerkt worden. Reden: " + e.getMessage();
				BerichtOntvangenLogEvent logEvent = new BerichtOntvangenLogEvent();
				logEvent.setBericht(ontvangenCdaBericht);
				logEvent.setMelding(melding);
				LOG.warn("Bericht " + berichtID + ": verwerkt met melding (zie logevent)");
				logService.logGebeurtenis(berichtType.getLbBerichtVerwerktMetMelding(), logEvent, berichtType.getBevolkingsonderzoek());
			}
		}
	}

	private Verslag getVerslagVoorSetID(OntvangenCdaBericht ontvangenCdaBericht, ClinicalDocument cdaDocument) throws OngeldigCdaException
	{
		String setId = ontvangenCdaBericht.getSetId();
		Verslag verslag = cdaVerslagDao.getVerslag(setId, ontvangenCdaBericht.getBerichtType().getVerslagType().getClazz());
		if (verslag != null)
		{
			String bsnNieuwBericht = getBsnFromDocument(cdaDocument);
			String bsnReedsVerwerkt = verslag.getScreeningRonde().getDossier().getClient().getPersoon().getBsn();
			if (!bsnReedsVerwerkt.equals(bsnNieuwBericht))
			{
				createOngeldigBerichtMelding(cdaDocument, ontvangenCdaBericht, verslag.getType(),
					"verslag met setId '" + setId + "' (v" + ontvangenCdaBericht.getVersie() + ") heeft een ander BSN (" + bsnNieuwBericht
						+ ") dan reeds verwerkt verslag met zelfde setId (v" + verslag.getOntvangenBericht().getVersie() + "), BSN " + bsnReedsVerwerkt
						+ CDAHelper.getUitvoerendeOrganisatieInformatie(cdaDocument),
					false);
			}
			if (!verslag.getType().equals(ontvangenCdaBericht.getBerichtType().getVerslagType()))
			{
				createOngeldigBerichtMelding(cdaDocument, ontvangenCdaBericht, verslag.getType(),
					"verslag met setId '" + setId + "' (v" + ontvangenCdaBericht.getVersie() + ") heeft een ander type (" + ontvangenCdaBericht.getBerichtType().getVerslagType()
						+ ") dan reeds verwerkt verslag met zelfde setId (v" + verslag.getOntvangenBericht().getVersie() + ") type " + verslag.getType() + ", BSN "
						+ bsnReedsVerwerkt + CDAHelper.getUitvoerendeOrganisatieInformatie(cdaDocument),
					false);
			}
		}
		return verslag;
	}

	private String getBsnFromDocument(ClinicalDocument cdaDocument)
	{
		List<II> ids = CDAHelper.getAllValues(cdaDocument, CdaConstants.PATIENT_IDS_PATH);
		return CDAHelper.getExtension(CdaOID.BSN, ids);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwerkError(Long berichtID, Exception e)
	{
		OntvangenCdaBericht ontvangenCdaBericht = hibernateService.load(OntvangenCdaBericht.class, berichtID);
		String melding = "Bericht " + berichtID + ": fout bij verwerking (" + e.getClass().getSimpleName() + ": " + e.getMessage() + ")";
		LOG.error(melding, e);

		if (ontvangenCdaBericht != null)
		{
			BerichtOntvangenLogEvent logEvent = new BerichtOntvangenLogEvent();
			logEvent.setBericht(ontvangenCdaBericht);
			logEvent.setMelding(melding);
			BerichtType berichtType = ontvangenCdaBericht.getBerichtType();
			logService.logGebeurtenis(berichtType.getLbBerichtVerwerktMetError(), logEvent, berichtType.getBevolkingsonderzoek());
		}
	}

	private boolean saveOrReplaceVerslag(Verslag verslag, ClinicalDocument cdaDocument) throws OngeldigCdaException
	{
		boolean vervangen = false;
		switch (verslag.getType())
		{
		case MDL:
		case PA_LAB:
			OntvangenCdaBericht ontvangenCdaBericht = verslag.getOntvangenBericht();
			Verslag bestaandeVerslag = getVerslagVoorSetID(ontvangenCdaBericht, cdaDocument);
			boolean verwijderd = false;
			OntvangenCdaBericht ontvangenCdaBerichtBestaandeVerslag = null;
			if (bestaandeVerslag != null)
			{
				ontvangenCdaBerichtBestaandeVerslag = bestaandeVerslag.getOntvangenBericht();
				if (bestaandeVerslag != null && ontvangenCdaBericht.getVersie().longValue() > ontvangenCdaBerichtBestaandeVerslag.getVersie().longValue())
				{
					if (VerslagType.MDL.equals(bestaandeVerslag.getType()))
					{
						verwerkVerslagService.ontkoppelOfVerwijderComplicaties((MdlVerslag) bestaandeVerslag);
					}

					ColonVerslag<?> colonVerslag = (ColonVerslag<?>) bestaandeVerslag;
					ColonScreeningRonde screeningRonde = colonVerslag.getScreeningRonde();
					screeningRonde.getVerslagen().remove(bestaandeVerslag);
					hibernateService.saveOrUpdate(screeningRonde);
					colonVerslag.setScreeningRonde(null);
					hibernateService.delete(bestaandeVerslag);

					verwijderd = true;
				}
			}
			if (bestaandeVerslag == null || verwijderd)
			{
				ColonVerslag<?> colonVerslag = (ColonVerslag<?>) verslag;
				ColonScreeningRonde screeningRonde = colonVerslag.getScreeningRonde();
				screeningRonde.getVerslagen().add(colonVerslag);
				hibernateService.saveOrUpdate(verslag);
				hibernateService.saveOrUpdate(screeningRonde);
				if (verwijderd)
				{
					String melding = "Verslag in provided document met berichtId " + ontvangenCdaBericht.getBerichtId() + ", setId " + ontvangenCdaBericht.getSetId()
						+ " en versie " + ontvangenCdaBericht.getVersie() + " vervangt verslag uit bericht met berichtId " + ontvangenCdaBerichtBestaandeVerslag.getBerichtId()
						+ ", setId " + ontvangenCdaBerichtBestaandeVerslag.getSetId() + " en versie " + ontvangenCdaBerichtBestaandeVerslag.getVersie();
					BerichtOntvangenLogEvent logEvent = new BerichtOntvangenLogEvent();
					logEvent.setBericht(ontvangenCdaBericht);
					logEvent.setMelding(melding);
					logService.logGebeurtenis(LogGebeurtenis.BERICHT_UITSLAG_VERVANGEN, logEvent, ontvangenCdaBericht.getBerichtType().getBevolkingsonderzoek());
					vervangen = true;
				}
			}
			break;
		case CERVIX_CYTOLOGIE:
			break;
		case MAMMA_PA_FOLLOW_UP:
			hibernateService.saveOrUpdate(verslag);
			break;
		}
		return vervangen;
	}

	private Verslag verwerkBvoSpecifiekeGegevens(Verslag verslag, ClinicalDocument cdaDocument) throws OngeldigCdaException
	{
		String bsn = getBsnFromDocument(cdaDocument);
		Client client = clientService.getClientByBsn(bsn);
		Verslag returnVerslag = verslag;
		if (client != null)
		{
			BerichtType berichtType = verslag.getOntvangenBericht().getBerichtType();

			switch (berichtType)
			{
			case MDL_VERSLAG:
			case PA_LAB_VERSLAG:

				Verslag olderVerslag = getOlderVerslag(verslag, cdaDocument);
				Date onderzoeksdatum = null;
				switch (berichtType)
				{
				case MDL_VERSLAG:
					onderzoeksdatum = getReferentieDatum(cdaDocument, CdaConstants.AANVANG_VERRICHTING_DATUM_MDL_PATH);
					break;
				case PA_LAB_VERSLAG:
					onderzoeksdatum = getReferentieDatum(cdaDocument, CdaConstants.AANVANG_VERRICHTING_DATUM_PA_PATH);
					break;
				}

				ScreeningRonde rondeVoorVerslag = verwerkVerslagService.getValideScreeningsRonde(verslag.getType(), client, olderVerslag, onderzoeksdatum);

				if (rondeVoorVerslag != null)
				{
					verslag.setScreeningRonde(rondeVoorVerslag);

					switch (berichtType)
					{
					case MDL_VERSLAG:
						verwerkMdlVerslagContent(cdaDocument, (MdlVerslag) verslag);
						break;
					case PA_LAB_VERSLAG:
						verwerkPaLabVerslagContent((PaVerslag) verslag);
						break;
					}

				}
				else
				{
					createOngeldigBerichtMelding(cdaDocument, verslag,
						"verslag met cli\u00EBntgegevens van een cli\u00EBnt heeft geen ongunstige uitslag, BSN: " + client.getPersoon().getBsn()
							+ CDAHelper.getUitvoerendeOrganisatieInformatie(cdaDocument),
						true);

				}
				break;
			case CERVIX_CYTOLOGIE_VERSLAG:
				List<II> monsterIdentificatieIds = CDAHelper.getAllValues(cdaDocument, CdaConstants.CYTOLOGIE_VERSLAG_MONSTER_IDENTIFICATIE_IDS);
				String monsterIdentificatie = CDAHelper.getExtension(CdaOID.CYTOLOGIE_VERSLAG_MONSTER_IDENTIFICATIE, monsterIdentificatieIds);
				if (monsterIdentificatie != null)
				{
					monsterIdentificatie = monsterIdentificatie.replaceFirst("^0+(?!$)", ""); 
				}
				CervixCytologieOrder cytologieOrder = bmhkLaboratoriumDao.getCytologieOrder(monsterIdentificatie);
				if (cytologieOrder == null)
				{
					createOngeldigBerichtMelding(cdaDocument, verslag,
						"verslag met cli\u00EBntgegevens van een cli\u00EBnt zonder dat een order bestaat, BSN: " + client.getPersoon().getBsn()
							+ CDAHelper.getUitvoerendeOrganisatieInformatie(cdaDocument),
						false);
				}
				else if (cytologieOrder.getStatus() != CervixCytologieOrderStatus.VERSTUURD)
				{
					createOngeldigBerichtMelding(cdaDocument, verslag,
						"verslag met cli\u00EBntgegevens van een cli\u00EBnt zonder dat een order verstuurd is, BSN: " + client.getPersoon().getBsn()
							+ CDAHelper.getUitvoerendeOrganisatieInformatie(cdaDocument),
						false);
				}
				else
				{
					CervixUitstrijkje uitstrijkje = cytologieOrder.getUitstrijkje();
					if (uitstrijkje.getCytologieVerslag() != null)
					{
						returnVerslag = uitstrijkje.getCytologieVerslag();
					}
					else if (!uitstrijkje.getOntvangstScreeningRonde().getDossier().getClient().equals(client))
					{
						createOngeldigBerichtMelding(cdaDocument, verslag,
							"verslag met monster-id dat hoort bij een ander BSN dan opgegeven in het bericht, BSN: " + client.getPersoon().getBsn()
								+ CDAHelper.getUitvoerendeOrganisatieInformatie(cdaDocument),
							false);
					}
					else
					{
						verslag.setScreeningRonde(uitstrijkje.getOntvangstScreeningRonde());
						verwerkCervixCytologieVerslagContent(cdaDocument, (CervixCytologieVerslag) verslag);
					}
				}

				break;
			case MAMMA_PA_FOLLOW_UP_VERSLAG:
				onderzoeksdatum = getReferentieDatum(cdaDocument, CdaConstants.AANVANG_VERRICHTING_DATUM_PA_PATH);

				rondeVoorVerslag = verwerkVerslagService.getValideScreeningsRonde(verslag.getType(), client, null, onderzoeksdatum);

				if (rondeVoorVerslag != null)
				{
					verslag.setScreeningRonde(rondeVoorVerslag);
					verwerkMammaFollowUpPaVerslagContent((MammaFollowUpVerslag) verslag);

				}
				else
				{
					createOngeldigBerichtMelding(cdaDocument, verslag,
						"verslag met cli\u00EBntgegevens van een cli\u00EBnt heeft geen beoordeling, BSN: " + client.getPersoon().getBsn()
							+ CDAHelper.getUitvoerendeOrganisatieInformatie(cdaDocument),
						true);
				}
				break;
			}
		}
		else
		{
			createOngeldigBerichtMelding(cdaDocument, verslag,
				"verslag met ongeldige cli\u00EBntgegevens, BSN: " + bsn + CDAHelper.getUitvoerendeOrganisatieInformatie(cdaDocument),
				false);
		}
		return returnVerslag;
	}

	private Verslag getOlderVerslag(Verslag newVerslag, ClinicalDocument cdaDocument) throws OngeldigCdaException
	{
		OntvangenCdaBericht ontvangenCdaBericht = newVerslag.getOntvangenBericht();
		Verslag bestaandeVerslag = null;
		if (ontvangenCdaBericht != null)
		{
			bestaandeVerslag = getVerslagVoorSetID(ontvangenCdaBericht, cdaDocument);
			if (bestaandeVerslag != null)
			{
				OntvangenCdaBericht ontvangenCdaBerichtBestaandeVerslag = bestaandeVerslag.getOntvangenBericht();
				if (bestaandeVerslag != null && ontvangenCdaBericht.getVersie().longValue() <= ontvangenCdaBerichtBestaandeVerslag.getVersie().longValue())
				{
					bestaandeVerslag = null;
				}
			}
		}
		return bestaandeVerslag;
	}

	private void createOngeldigBerichtMelding(ClinicalDocument cdaDocument, Verslag verslag, String message, boolean herstelbaar) throws OngeldigCdaException
	{
		createOngeldigBerichtMelding(cdaDocument, verslag.getOntvangenBericht(), verslag.getType(), message, herstelbaar);
	}

	private void createOngeldigBerichtMelding(ClinicalDocument cdaDocument, OntvangenCdaBericht ontvangenCdaBericht, VerslagType verslagType, String message, boolean herstelbaar)
		throws OngeldigCdaException
	{
		MeldingOngeldigCdaBericht melding = new MeldingOngeldigCdaBericht();
		melding.setOntvangenCdaBericht(ontvangenCdaBericht);
		Instelling instelling = null;
		if (verslagType.equals(VerslagType.MDL))
		{
			instelling = getInstellingFromCda(cdaDocument, OrganisatieType.COLOSCOPIELOCATIE, OrganisatieType.ZORGINSTELLING);
		}
		else if (verslagType.equals(VerslagType.PA_LAB))
		{
			instelling = getInstellingFromCda(cdaDocument, OrganisatieType.PA_LABORATORIUM);
		}
		else if (verslagType.equals(VerslagType.CERVIX_CYTOLOGIE))
		{
			instelling = getInstellingFromCda(cdaDocument, OrganisatieType.BMHK_LABORATORIUM);
		}
		melding.setUitvoerder(getMedewerkerFromCda(cdaDocument, verslagType, instelling));
		melding.setHerstelbaar(herstelbaar);

		melding.setUitvoerendeOrganisatie(instelling);
		melding.setActief(true);
		melding.setDatum(currentDateSupplier.getDate());
		melding.setBsn(getBsnFromDocument(cdaDocument));

		if (instelling != null)
		{
			ScreeningOrganisatie screeningOrganisatie = null;
			if (instelling.getRegio() != null)
			{
				screeningOrganisatie = hibernateService.get(ScreeningOrganisatie.class, instelling.getRegio().getId());
			}
			else
			{
				Instelling parent = instelling.getParent();
				if (parent != null && OrganisatieType.SCREENINGSORGANISATIE.equals(parent.getOrganisatieType()))
				{
					screeningOrganisatie = hibernateService.get(ScreeningOrganisatie.class, parent.getId());
				}
			}
			melding.setScreeningOrganisatie(screeningOrganisatie);
		}

		melding.setMelding(message);
		hibernateService.saveOrUpdate(melding);
		throw new OngeldigCdaException(message);
	}

	private void verwerkPaLabVerslagContent(PaVerslag verslag)
	{
		verwerkCdaBerichtContentService.verwerkVerslagContent(verslag, PaVerslagContent.class);
	}

	private void verwerkMdlVerslagContent(ClinicalDocument cdaDocument, MdlVerslag verslag) throws OngeldigCdaException
	{
		List<II> ids = CDAHelper.getAllValues(cdaDocument, CdaConstants.PATIENT_IDS_PATH);
		for (II id : ids)
		{
			if (id.getRoot() != null && !CdaOID.BSN.equals(id.getRoot()))
			{
				if (id.getExtension() != null && StringUtils.isBlank(id.getExtension()))
				{
					createOngeldigBerichtMelding(cdaDocument, verslag,
						"verslag ongeldig lokaal patientnummer, BSN " + verslag.getScreeningRonde().getDossier().getClient().getPersoon().getBsn(), false);
				}
				String patientnummer = CDAHelper.getRootExtension(id);

				String[] splittedPatientnummer = patientnummer.split("\\.");
				ArrayUtils.reverse(splittedPatientnummer);
				for (String patientnummerPart : splittedPatientnummer)
				{
					if (StringUtils.isNotBlank(patientnummerPart))
					{
						patientnummer = patientnummerPart;
						break;
					}
				}
				verslag.setPatientnummer(patientnummer);
				break;
			}
		}

		verwerkCdaBerichtContentService.verwerkVerslagContent(verslag, MdlVerslagContent.class);
	}

	private void verwerkCervixCytologieVerslagContent(ClinicalDocument cdaDocument, CervixCytologieVerslag cytologieVerslag) throws OngeldigCdaException
	{
		verwerkCdaBerichtContentService.verwerkVerslagContent(cytologieVerslag, CervixCytologieVerslagContent.class);
	}

	private void verwerkMammaFollowUpPaVerslagContent(MammaFollowUpVerslag verslag)
	{
		verwerkCdaBerichtContentService.verwerkVerslagContent(verslag, MammaFollowUpVerslagContent.class);
	}

	private void verwerkUitvoerderInVerslag(Verslag verslag, ClinicalDocument cdaDocument) throws OngeldigCdaException
	{
		if (verslag instanceof MdlVerslag)
		{
			getMdlUitvoerder(cdaDocument, verslag);
		}
		else if (verslag instanceof PaVerslag)
		{
			getPaLabUitvoerder(cdaDocument, verslag);
		}
		else if (verslag instanceof CervixCytologieVerslag)
		{
			CervixCytologieVerslag deproxiedVerslag = (CervixCytologieVerslag) HibernateHelper.deproxy(verslag);
			getCervixCytologieUitvoerder(cdaDocument, deproxiedVerslag);
		}
		else if (verslag instanceof MammaFollowUpVerslag)
		{
			getFollowUpLaboratorium(cdaDocument, verslag);
		}
	}

	private void getFollowUpLaboratorium(ClinicalDocument cdaDocument, Verslag verslag) throws OngeldigCdaException
	{
		POCDMT000040AssignedEntity assignedAuthor = CDAHelper.getAssigendEntity(cdaDocument);

		StringBuilder organisationId = new StringBuilder();
		getOrganisatie(assignedAuthor, organisationId);

		((MammaVerslag) verslag).setLabCode(organisationId.toString());
	}

	private void getPaLabUitvoerder(ClinicalDocument cdaDocument, Verslag verslag) throws OngeldigCdaException
	{
		POCDMT000040AssignedEntity assignedAuthor = CDAHelper.getAssigendEntity(cdaDocument);

		Instelling instelling = null;
		StringBuilder organisationId = new StringBuilder();
		instelling = getOrganisatie(assignedAuthor, organisationId, OrganisatieType.PA_LABORATORIUM);

		if (instelling == null)
		{
			String message = "verslag ";
			if (organisationId.length() > 0)
			{
				message += "met onbekende PA lab identificatie(s) " + organisationId.toString();
			}
			else
			{
				message += "zonder geldige PA lab identificatie";
			}
			message += " (" + CDAHelper.getFirstValueNotNull(assignedAuthor, CommonCdaConstants.ORGANIZATION_NAME_SUBPATH) + ")";
			createOngeldigBerichtMelding(cdaDocument, verslag, message, true);
		}

		if (!isPabLabGekoppeldAanZorginstelling(instelling))
		{
			String message = "verslag waarbij een PA lab gevonden met een van de identificatie(s) " + organisationId.toString() + " niet gekoppeld is aan een zorginstelling";
			message += " (" + CDAHelper.getFirstValueNotNull(assignedAuthor, CommonCdaConstants.ORGANIZATION_NAME_SUBPATH) + ")";
			createOngeldigBerichtMelding(cdaDocument, verslag, message, true);
		}

		InstellingGebruiker instellingMedewerker = null;

		String patholoogId = getPatholoogId(cdaDocument);
		if (StringUtils.isBlank(patholoogId))
		{
			String message = "verslag zonder geldige patholoog identificatie";
			message += " (" + CDAHelper.getFirstValueNotNull(assignedAuthor, CommonCdaConstants.ORGANIZATION_NAME_SUBPATH) + ")";
			createOngeldigBerichtMelding(cdaDocument, verslag, message, false);
		}
		if (CollectionUtils.isNotEmpty(instelling.getOrganisatieMedewerkers()) && patholoogId != null)
		{
			for (InstellingGebruiker ig : instelling.getOrganisatieMedewerkers())
			{
				if (Boolean.TRUE.equals(ig.getActief()))
				{
					Gebruiker medewerker = ig.getMedewerker();
					if (MedewerkerUtil.isMedewerkerActief(medewerker, currentDateSupplier.getDate()) && patholoogId != null && patholoogId.equals(medewerker.getPatholoogId()))
					{
						instellingMedewerker = ig;
						break;
					}
				}
			}
		}
		if (instellingMedewerker == null)
		{
			String melding = "verslag ";
			if (StringUtils.isNotBlank(patholoogId))
			{
				melding += "met ongeldige patholoog identificatie " + patholoogId;
			}

			if (assignedAuthor != null)
			{
				POCDMT000040Person assignedPerson = assignedAuthor.getAssignedPerson();
				if (assignedPerson != null)
				{
					List<PN> names = assignedPerson.getNames();
					String namePart = CDAHelper.getNamePart(names, EnPrefix.class, -1, "") + " " + CDAHelper.getNamePart(names, EnFamily.class, -1, "onbekend") + ", "
						+ CDAHelper.getNamePart(names, EnGiven.class, -1, "");
					melding += " (" + namePart.trim() + ")";
				}

			}
			if (StringUtils.isNotBlank(organisationId.toString()))
			{
				melding += " (binnen pathologielaboratorium met identificatie " + organisationId.toString() + ")";
			}
			createOngeldigBerichtMelding(cdaDocument, verslag, melding, true);
		}
		else
		{
			boolean vervanging = getOlderVerslag(verslag, cdaDocument) != null;

			Date overeenkomstPeildatum = getReferentieDatum(cdaDocument, CdaConstants.AANVANG_VERRICHTING_DATUM_PA_PATH);
			boolean hasActiveKwaliteitsovereenkomst = kwaliteitsovereenkomstDao.hasActiveKwaliteitsovereenkomst(instellingMedewerker.getMedewerker(), overeenkomstPeildatum);
			if (vervanging || hasActiveKwaliteitsovereenkomst)
			{
				verslag.setUitvoerderOrganisatie(instellingMedewerker.getOrganisatie());
				verslag.setUitvoerderMedewerker(instellingMedewerker.getMedewerker());
			}
			else
			{
				String melding = "Patholoog " + instellingMedewerker.getMedewerker().getNaamVolledig() + " (" + instellingMedewerker.getOrganisatie().getNaam()
					+ ") met identificatie '" + patholoogId + "' heeft geen (actieve) overeenkomst";
				createOngeldigBerichtMelding(cdaDocument, verslag, melding, true);
			}
		}
	}

	private void getMdlUitvoerder(ClinicalDocument cdaDocument, Verslag verslag) throws OngeldigCdaException
	{
		POCDMT000040AssignedEntity assignedAuthor = CDAHelper.getAssigendEntity(cdaDocument);

		StringBuilder organisationId = new StringBuilder();
		Instelling instelling = getOrganisatie(assignedAuthor, organisationId, OrganisatieType.COLOSCOPIELOCATIE, OrganisatieType.ZORGINSTELLING);

		if (instelling == null)
		{
			String message = "verslag ";
			if (organisationId.length() > 0)
			{
				message += "met ongeldige coloscopielocatie/zorginstelling identificatie(s) " + organisationId.toString();
			}
			else
			{
				message += "zonder geldige coloscopielocatie/zorginstelling identificatie";
			}
			message += " (" + CDAHelper.getFirstValueNotNull(assignedAuthor, CommonCdaConstants.ORGANIZATION_NAME_SUBPATH) + ")";
			createOngeldigBerichtMelding(cdaDocument, verslag, message, true);
		}
		Gebruiker instellingMedewerker = null;

		String uzinummer = CDAHelper.getUzinummer(cdaDocument);
		if (StringUtils.isNotBlank(uzinummer))
		{
			if (OrganisatieType.ZORGINSTELLING.equals(instelling.getOrganisatieType()))
			{
				for (Instelling child : instelling.getChildren())
				{
					if (!Boolean.FALSE.equals(child.getActief()) && OrganisatieType.COLOSCOPIELOCATIE.equals(child.getOrganisatieType()))
					{
						instellingMedewerker = findMedewerker(child, uzinummer);
						if (instellingMedewerker != null)
						{
							break;
						}
					}
				}
			}
			else if (OrganisatieType.COLOSCOPIELOCATIE.equals(instelling.getOrganisatieType()))
			{
				instellingMedewerker = findMedewerker(instelling, uzinummer);
			}

		}
		if (instellingMedewerker == null)
		{
			String message = "verslag ";
			if (StringUtils.isNotBlank(uzinummer))
			{
				message += "met ongeldig medewerker uzinummer " + uzinummer;
			}
			else
			{
				message += "zonder geldige medewerker uzinummer";
			}
			if (assignedAuthor != null)
			{
				POCDMT000040Person assignedPerson = assignedAuthor.getAssignedPerson();
				if (assignedPerson != null)
				{
					List<PN> names = assignedPerson.getNames();
					String namePart = CDAHelper.getNamePart(names, EnPrefix.class, -1, "") + " " + CDAHelper.getNamePart(names, EnFamily.class, -1, "onbekend") + ", "
						+ CDAHelper.getNamePart(names, EnGiven.class, -1, "");
					message += " (" + namePart.trim() + ")";
				}

			}
			if (StringUtils.isNotBlank(organisationId.toString()))
			{
				message += " (binnen coloscopielocatie/zorginstelling met identificatie " + organisationId.toString() + ")";
			}

			createOngeldigBerichtMelding(cdaDocument, verslag, message, true);
		}
		else
		{
			boolean vervanging = getOlderVerslag(verslag, cdaDocument) != null;

			Date overeenkomstPeildatum = getReferentieDatum(cdaDocument, CdaConstants.AANVANG_VERRICHTING_DATUM_MDL_PATH);
			boolean hasActiveKwaliteitsovereenkomst = kwaliteitsovereenkomstDao.hasActiveKwaliteitsovereenkomst(instellingMedewerker, overeenkomstPeildatum);
			if (vervanging || hasActiveKwaliteitsovereenkomst)
			{
				verslag.setUitvoerderOrganisatie(instelling);
				verslag.setUitvoerderMedewerker(instellingMedewerker);
			}
			else
			{
				String melding = "Medewerker " + instellingMedewerker.getNaamVolledig() + " (" + instelling.getNaam() + ") met uzinummer " + uzinummer
					+ " heeft geen (actieve) overeenkomst";
				createOngeldigBerichtMelding(cdaDocument, verslag, melding, true);
			}
		}
	}

	private void getCervixCytologieUitvoerder(ClinicalDocument cdaDocument, CervixCytologieVerslag verslag) throws OngeldigCdaException
	{
		POCDMT000040AssignedEntity assignedAuthor = CDAHelper.getAssigendEntity(cdaDocument);
		StringBuilder organisationId = new StringBuilder();

		Instelling instelling = getOrganisatie(assignedAuthor, organisationId, OrganisatieType.BMHK_LABORATORIUM);
		if (instelling == null)
		{
			String message = "verslag ";
			boolean herstelbaar;
			if (organisationId.length() > 0)
			{
				message += "met onbekende BMHK laboratorium identificatie(s) " + organisationId.toString();
				herstelbaar = true;
			}
			else
			{
				message += "zonder geldige BMHK laboratorium identificatie";
				herstelbaar = false;
			}
			message += " (" + CDAHelper.getFirstValueNotNull(assignedAuthor, CommonCdaConstants.ORGANIZATION_NAME_SUBPATH) + ")";
			createOngeldigBerichtMelding(cdaDocument, verslag, message, herstelbaar);
		}

		String patholoogId = getPatholoogId(cdaDocument);
		if (StringUtils.isBlank(patholoogId))
		{
			String message = "verslag zonder geldige patholoog identificatie";
			message += " (" + CDAHelper.getFirstValueNotNull(assignedAuthor, CommonCdaConstants.ORGANIZATION_NAME_SUBPATH) + ")";
			createOngeldigBerichtMelding(cdaDocument, verslag, message, false);
		}

		String patholoogNaam = getPatholoogNaam(cdaDocument);
		if (StringUtils.isBlank(patholoogNaam))
		{
			String message = "verslag zonder geldige patholoog ondertekening";
			createOngeldigBerichtMelding(cdaDocument, verslag, message, false);
		}

		InstellingGebruiker instellingGebruiker = null;
		if (CollectionUtils.isNotEmpty(instelling.getOrganisatieMedewerkers()) && patholoogId != null)
		{
			for (InstellingGebruiker ig : instelling.getOrganisatieMedewerkers())
			{
				if (Boolean.TRUE.equals(ig.getActief()))
				{
					Gebruiker medewerker = ig.getMedewerker();
					if (Boolean.TRUE.equals(medewerker.getActief()) && patholoogId != null && patholoogId.equals(medewerker.getPatholoogId()))
					{
						instellingGebruiker = ig;
						break;
					}
				}
			}
		}
		if (instellingGebruiker == null)
		{
			String melding = "verslag ";
			if (StringUtils.isNotBlank(patholoogId))
			{
				melding += "met ongeldige patholoog identificatie " + patholoogId;
			}

			if (assignedAuthor != null)
			{
				POCDMT000040Person assignedPerson = assignedAuthor.getAssignedPerson();
				if (assignedPerson != null)
				{
					List<PN> names = assignedPerson.getNames();
					String namePart = CDAHelper.getNamePart(names, EnPrefix.class, -1, "") + " " + CDAHelper.getNamePart(names, EnFamily.class, -1, "onbekend") + ", "
						+ CDAHelper.getNamePart(names, EnGiven.class, -1, "");
					melding += " (" + namePart.trim() + ")";
				}
			}

			if (StringUtils.isNotBlank(organisationId.toString()))
			{
				melding += " (binnen BMHK laboratorium met identificatie " + organisationId.toString() + ")";
			}
			createOngeldigBerichtMelding(cdaDocument, verslag, melding, true);
		}
		else
		{
			verslag.setPatholoogNaam(patholoogNaam);
			verslag.setUitvoerderOrganisatie(instellingGebruiker.getOrganisatie());
			verslag.setUitvoerderMedewerker(instellingGebruiker.getMedewerker());
		}
	}

	private String getPatholoogNaam(ClinicalDocument cdaDocument)
	{
		POCDMT000040AssignedAuthor assignedAuthor = CDAHelper.getAssigendAuthor(cdaDocument);
		if (assignedAuthor != null)
		{
			return CDAHelper.getFirstValueNotNull(assignedAuthor, CommonCdaConstants.PATHALOOG_AUTHOR_NAME_SUB_PATH);
		}
		return null;
	}

	private static Date getReferentieDatum(ClinicalDocument cdaDocument, String onderzoekdatumPath)
	{
		Date onderzoekDatum = null;
		try
		{
			onderzoekDatum = CDAHelper.converCdaDateStringToDate((String) CDAHelper.getFirstValue(cdaDocument, onderzoekdatumPath));
		}
		catch (ParseException e)
		{
			LOG.error("Fout bij ophalen referentiedatum " + e.getMessage());
		}
		return onderzoekDatum;
	}

	private boolean isPabLabGekoppeldAanZorginstelling(Instelling instelling)
	{
		PaLaboratorium pa = null;
		Boolean gekoppeld = false;
		if (OrganisatieType.PA_LABORATORIUM.equals(instelling.getOrganisatieType()))
		{
			pa = (PaLaboratorium) instelling;
		}
		if (pa != null && pa.getColoscopielocaties().size() > 0)
		{
			for (ColoscopieLocatie col : pa.getColoscopielocaties())
			{
				Instelling zorginstelling = col.getParent();
				if (zorginstelling != null && OrganisatieType.ZORGINSTELLING.equals(zorginstelling.getOrganisatieType()))
				{
					gekoppeld = true;
				}
			}
		}
		return gekoppeld;
	}

	private Gebruiker findMedewerker(Instelling instelling, String uzinummer)
	{
		Gebruiker instellingMedewerker = null;
		if (CollectionUtils.isNotEmpty(instelling.getOrganisatieMedewerkers()))
		{
			for (InstellingGebruiker ig : instelling.getOrganisatieMedewerkers())
			{
				Gebruiker medewerker = ig.getMedewerker();
				if (Boolean.TRUE.equals(ig.getActief()) && 
					Boolean.TRUE.equals(medewerker.getActief()) && 
					uzinummer.equals(medewerker.getUzinummer()) 
				)
				{
					instellingMedewerker = medewerker;
					break;
				}
			}
		}
		return instellingMedewerker;
	}

	private Instelling getOrganisatie(POCDMT000040AssignedEntity assignedAuthor, StringBuilder organisationId, OrganisatieType... types)
	{
		Instelling instelling = null;

		if (assignedAuthor != null)
		{
			POCDMT000040Organization representedOrganization = assignedAuthor.getRepresentedOrganization();
			if (representedOrganization != null && representedOrganization.getIds() != null)
			{
				for (II ii : representedOrganization.getIds())
				{
					Instelling foundInstelling = null;
					if (ii.getRoot() != null)
					{
						String root = ii.getRoot();
						String extension = ii.getExtension();
						if (CommonCdaConstants.URA.equals(root))
						{
							if (StringUtils.isNotBlank(extension))
							{
								foundInstelling = instellingService.getInstellingBy("uziAbonneenummer", StringUtils.leftPad(extension.trim(), 8, '0'));

								if (organisationId.length() > 0)
								{
									organisationId.append(", ");
								}
								organisationId.append("URA:");
								organisationId.append(extension);
							}
						}
						else
						{
							String organisationOid = null;
							if (StringUtils.isNotBlank(root))
							{
								organisationOid = root;
								if (StringUtils.isNotBlank(extension))
								{
									organisationOid += "." + extension.trim();
								}
							}
							if (organisationId != null)
							{
								foundInstelling = instellingService.getInstellingBy("rootOid", organisationOid);
								if (organisationId.length() > 0)
								{
									organisationId.append(", ");
								}
								organisationId.append(organisationOid);
							}
						}
					}
					if (foundInstelling != null && Arrays.asList(types).contains(foundInstelling.getOrganisatieType()))
					{
						instelling = foundInstelling;
					}

					if (instelling != null && !OrganisatieType.ZORGINSTELLING.equals(instelling.getOrganisatieType()))
					{
						break;
					}
				}
			}
		}

		if (instelling != null && OrganisatieType.ZORGINSTELLING.equals(instelling.getOrganisatieType()))
		{
			ZorgInstelling zorgInstelling = (ZorgInstelling) instelling;
			ColoscopieLocatie locatie = null;
			for (Instelling child : zorgInstelling.getChildren())
			{
				if (OrganisatieType.COLOSCOPIELOCATIE.equals(child.getOrganisatieType()))
				{

					if (locatie == null)
					{
						locatie = (ColoscopieLocatie) child;
						instelling = child;
					}
					else
					{

						instelling = null;
						break;
					}
				}
			}
		}
		return instelling;
	}

	private Instelling getInstellingFromCda(ClinicalDocument cda, OrganisatieType... types)
	{
		POCDMT000040AssignedEntity assignedAuthor = CDAHelper.getAssigendEntity(cda);
		StringBuilder organisationId = new StringBuilder();

		return getOrganisatie(assignedAuthor, organisationId, types);
	}

	private String getPaLabId(POCDMT000040AssignedEntity assignedAuthor)
	{
		String paLabId = null;
		if (assignedAuthor != null)
		{
			POCDMT000040Organization representedOrganization = assignedAuthor.getRepresentedOrganization();
			if (representedOrganization != null)
			{
				for (II ii : representedOrganization.getIds())
				{
					if (!CommonCdaConstants.URA.equals(ii.getRoot()))
					{
						paLabId = CDAHelper.getRootExtension(ii);
					}
					if (paLabId != null)
					{
						break;
					}
				}
			}
		}
		return paLabId;
	}

	private Gebruiker getMedewerkerFromCda(ClinicalDocument cda, VerslagType verslagType, Instelling instelling)
	{
		Gebruiker medewerker = null;
		switch (verslagType)
		{
		case MDL:
			String uzinummer = CDAHelper.getUzinummer(cda);

			if (StringUtils.isNotBlank(uzinummer))
			{
				medewerker = gebruikersService.getGebruikerBy("uzinummer", uzinummer);
			}
			break;
		case PA_LAB:
		case CERVIX_CYTOLOGIE:
			String patholoogId = getPatholoogId(cda);

			if (StringUtils.isNotBlank(patholoogId))
			{
				medewerker = gebruikersService.getPatholoog(patholoogId, instelling);
			}
			break;
		}
		return medewerker;
	}

	private String getPatholoogId(ClinicalDocument cda)
	{
		POCDMT000040AssignedEntity assignedAuthor = CDAHelper.getAssigendEntity(cda);
		String patholoogId = null;
		if (assignedAuthor != null)
		{
			String paLabId = getPaLabId(assignedAuthor);
			if (StringUtils.isNotBlank(paLabId))
			{
				patholoogId = CDAHelper.getExtension(paLabId + ".1", assignedAuthor.getIds());
			}
		}
		return patholoogId;
	}

}
