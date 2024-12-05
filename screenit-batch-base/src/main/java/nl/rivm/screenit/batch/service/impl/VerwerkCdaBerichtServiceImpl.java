package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.exceptions.OngeldigCdaException;
import nl.rivm.screenit.batch.service.VerwerkCdaBerichtContentService;
import nl.rivm.screenit.batch.service.VerwerkCdaBerichtService;
import nl.rivm.screenit.hl7v3.cda.ClinicalDocument;
import nl.rivm.screenit.hl7v3.cda.EnFamily;
import nl.rivm.screenit.hl7v3.cda.EnGiven;
import nl.rivm.screenit.hl7v3.cda.EnPrefix;
import nl.rivm.screenit.hl7v3.cda.II;
import nl.rivm.screenit.hl7v3.cda.POCDMT000040AssignedEntity;
import nl.rivm.screenit.hl7v3.cda.helper.CDAHelper;
import nl.rivm.screenit.hl7v3.cda.helper.CommonCdaConstants;
import nl.rivm.screenit.hl7v3.cda.helper.ExtractCDA;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.ScreeningOrganisatie;
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
import nl.rivm.screenit.model.cervix.CervixCytologieOrder_;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieVerslagContent;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaLaboratorium;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.colon.verslag.pa.PaVerslagContent;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.BerichtOntvangenLogEvent;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.verslag.MammaVerslag;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpVerslagContent;
import nl.rivm.screenit.repository.algemeen.VerslagRepository;
import nl.rivm.screenit.repository.cervix.CervixCytologieOrderRepository;
import nl.rivm.screenit.service.BaseCdaVerslagService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.GebruikersService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.KwaliteitsovereenkomstService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.VerwerkVerslagService;
import nl.rivm.screenit.util.MedewerkerUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftMonsterId;

@Service
@Slf4j
public class VerwerkCdaBerichtServiceImpl implements VerwerkCdaBerichtService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseCdaVerslagService baseCdaVerslagService;

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
	private KwaliteitsovereenkomstService kwaliteitsovereenkomstService;

	@Autowired
	private CervixCytologieOrderRepository cytologieOrderRepository;

	@Autowired
	private VerslagRepository verslagRepository;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwerkBericht(Long berichtID)
	{
		LOG.info("Bericht " + berichtID + ": start verwerking.");
		var ontvangenCdaBericht = hibernateService.load(OntvangenCdaBericht.class, berichtID);
		ontvangenCdaBericht.setStatus(BerichtStatus.VERWERKT);
		hibernateService.saveOrUpdate(ontvangenCdaBericht);
		var cdaDocument = ExtractCDA.getCDADocument(ontvangenCdaBericht.getXmlBericht());
		var berichtType = ontvangenCdaBericht.getBerichtType();
		try
		{
			var bestaandeVerslag = verslagRepository.getVerslagVoorBerichtId(ontvangenCdaBericht.getBerichtId(),
				ontvangenCdaBericht.getBerichtType().getVerslagType().getClazz());
			if (bestaandeVerslag.isPresent())
			{
				berichtOpnieuwVerwerken(berichtID, cdaDocument, berichtType, bestaandeVerslag.get());
			}
			else
			{
				verwerkBericht(ontvangenCdaBericht, cdaDocument);
			}
		}
		catch (OngeldigCdaException e)
		{
			var melding = "CDA met berichtId " + ontvangenCdaBericht.getBerichtId() + ", setId " + ontvangenCdaBericht.getSetId() + " en versie "
				+ ontvangenCdaBericht.getVersie() + " moet handmatig verwerkt worden. Reden: " + e.getMessage();
			var logEvent = new BerichtOntvangenLogEvent();
			logEvent.setBericht(ontvangenCdaBericht);
			logEvent.setMelding(melding);
			LOG.warn("Bericht " + berichtID + ": verwerkt met melding (zie logevent)");
			logService.logGebeurtenis(berichtType.getLbBerichtVerwerktMetMelding(), logEvent, berichtType.getBevolkingsonderzoek());
		}
	}

	private void berichtOpnieuwVerwerken(Long berichtID, ClinicalDocument cdaDocument, BerichtType berichtType, Verslag bestaandeVerslag) throws OngeldigCdaException
	{
		LOG.warn("Bericht " + berichtID + ": opnieuw verwerken. Vervang content in bestaande verslag (id: " + bestaandeVerslag.getId() + ")");
		switch (berichtType)
		{
		case MDL_VERSLAG:
			var mdlVerslag = (MdlVerslag) bestaandeVerslag;
			verwerkVerslagService.ontkoppelOfVerwijderComplicaties(mdlVerslag);
			verwerkMdlVerslagContent(cdaDocument, mdlVerslag);
			break;
		case PA_LAB_VERSLAG:
			verwerkPaLabVerslagContent((PaVerslag) bestaandeVerslag);
			break;
		case CERVIX_CYTOLOGIE_VERSLAG:
			LOG.warn("Bericht " + berichtID + ": nogmaals verwerken niet mogelijk. Geskipped.");
			return;
		case MAMMA_PA_FOLLOW_UP_VERSLAG:
			verwerkMammaFollowUpPaVerslagContent((MammaFollowUpVerslag) bestaandeVerslag);
			break;
		}
		bestaandeVerslag.setDatumVerwerkt(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(bestaandeVerslag);
		verwerkVerslagService.verwerkInDossier(bestaandeVerslag);
		verwerkVerslagService.onAfterVerwerkVerslagContent(bestaandeVerslag);
		LOG.warn("Bericht " + berichtID + ": opnieuw verwerkt.");
	}

	private void verwerkBericht(OntvangenCdaBericht ontvangenCdaBericht, ClinicalDocument cdaDocument) throws OngeldigCdaException
	{
		List<Instelling> instellingen = new ArrayList<>();
		instellingen.add(hibernateService.loadAll(Rivm.class).get(0));
		Verslag verslag = null;
		var berichtType = ontvangenCdaBericht.getBerichtType();
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
		verwerkVerslagService.verwerkInDossier(verslag);
		verwerkVerslagService.onAfterVerwerkVerslagContent(verslag);

		var uitvoerendeInstelling = verslag.getUitvoerderOrganisatie();
		var melding = "CDA met berichtId " + ontvangenCdaBericht.getBerichtId() + ", setId " + ontvangenCdaBericht.getSetId() + " en versie "
			+ ontvangenCdaBericht.getVersie() + " is verwerkt. Uitvoerder: "
			+ (uitvoerendeInstelling != null ? uitvoerendeInstelling.getNaam() : ((MammaVerslag) verslag).getLabCode());
		var logEvent = new BerichtOntvangenLogEvent();
		logEvent.setBericht(ontvangenCdaBericht);
		logEvent.setMelding(melding);

		if (uitvoerendeInstelling != null)
		{
			instellingen.add(uitvoerendeInstelling);
		}
		logService.logGebeurtenis(berichtType.getLbBerichtVerwerkt(), instellingen, logEvent, null, verslag.getScreeningRonde().getDossier().getClient(),
			verslag.getType().getBevolkingsonderzoek());
		LOG.info("Bericht " + ontvangenCdaBericht.getId() + ": verwerkt.");

	}

	private Verslag getVerslagVoorSetID(OntvangenCdaBericht ontvangenCdaBericht, ClinicalDocument cdaDocument) throws OngeldigCdaException
	{
		var setId = ontvangenCdaBericht.getSetId();
		var verslag = baseCdaVerslagService.getVerslag(setId, ontvangenCdaBericht.getBerichtType().getVerslagType().getClazz());
		if (verslag != null)
		{
			var bsnNieuwBericht = getBsnFromDocument(cdaDocument);
			var bsnReedsVerwerkt = verslag.getScreeningRonde().getDossier().getClient().getPersoon().getBsn();
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
		var ontvangenCdaBericht = hibernateService.load(OntvangenCdaBericht.class, berichtID);
		var melding = "Bericht " + berichtID + ": fout bij verwerking (" + e.getClass().getSimpleName() + ": " + e.getMessage() + ")";
		LOG.error(melding, e);

		if (ontvangenCdaBericht != null)
		{
			ontvangenCdaBericht.setStatus(BerichtStatus.VERWERKT);
			hibernateService.saveOrUpdate(ontvangenCdaBericht);
			var logEvent = new BerichtOntvangenLogEvent();
			logEvent.setBericht(ontvangenCdaBericht);
			logEvent.setMelding(melding);
			var berichtType = ontvangenCdaBericht.getBerichtType();
			logService.logGebeurtenis(berichtType.getLbBerichtVerwerktMetError(), logEvent, berichtType.getBevolkingsonderzoek());
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public List<Long> getAlleNietVerwerkteCdaBerichten(Bevolkingsonderzoek bvo)
	{
		return baseCdaVerslagService.getAlleNietVerwerkteCdaBerichten(bvo);
	}

	private boolean saveOrReplaceVerslag(Verslag verslag, ClinicalDocument cdaDocument) throws OngeldigCdaException
	{
		var vervangen = false;
		switch (verslag.getType())
		{
		case MDL:
		case PA_LAB:
			var ontvangenCdaBericht = verslag.getOntvangenBericht();
			var bestaandeVerslag = getVerslagVoorSetID(ontvangenCdaBericht, cdaDocument);
			var verwijderd = false;
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

					var colonVerslag = (ColonVerslag<?>) bestaandeVerslag;
					var screeningRonde = colonVerslag.getScreeningRonde();
					screeningRonde.getVerslagen().remove(bestaandeVerslag);
					hibernateService.saveOrUpdate(screeningRonde);
					bestaandeVerslag.setScreeningRonde(null);
					hibernateService.delete(bestaandeVerslag);

					verwijderd = true;
				}
			}
			if (bestaandeVerslag == null || verwijderd)
			{
				var colonVerslag = (ColonVerslag<?>) verslag;
				var screeningRonde = colonVerslag.getScreeningRonde();
				screeningRonde.getVerslagen().add(colonVerslag);
				hibernateService.saveOrUpdate(verslag);
				hibernateService.saveOrUpdate(screeningRonde);
				if (verwijderd)
				{
					var melding = "Verslag in provided document met berichtId " + ontvangenCdaBericht.getBerichtId() + ", setId " + ontvangenCdaBericht.getSetId()
						+ " en versie " + ontvangenCdaBericht.getVersie() + " vervangt verslag uit bericht met berichtId " + ontvangenCdaBerichtBestaandeVerslag.getBerichtId()
						+ ", setId " + ontvangenCdaBerichtBestaandeVerslag.getSetId() + " en versie " + ontvangenCdaBerichtBestaandeVerslag.getVersie();
					var logEvent = new BerichtOntvangenLogEvent();
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
		var bsn = getBsnFromDocument(cdaDocument);
		var client = clientService.getClientByBsn(bsn);
		var returnVerslag = verslag;
		if (client != null)
		{
			var berichtType = verslag.getOntvangenBericht().getBerichtType();

			switch (berichtType)
			{
			case MDL_VERSLAG:
			case PA_LAB_VERSLAG:

				var olderVerslag = getOlderVerslag(verslag, cdaDocument);
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

				var rondeVoorVerslag = verwerkVerslagService.getValideScreeningsRonde(verslag.getType(), client, olderVerslag, onderzoeksdatum);

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
				var monsterIdentificatie = CDAHelper.getExtension(CdaOID.CYTOLOGIE_VERSLAG_MONSTER_IDENTIFICATIE, monsterIdentificatieIds);
				if (monsterIdentificatie != null)
				{
					monsterIdentificatie = monsterIdentificatie.replaceFirst("^0+(?!$)", ""); 
				}
				var cytologieOrder = cytologieOrderRepository.findOne(heeftMonsterId(monsterIdentificatie).with(CervixCytologieOrder_.uitstrijkje))
					.orElse(null);
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
					var uitstrijkje = cytologieOrder.getUitstrijkje();
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
		var ontvangenCdaBericht = newVerslag.getOntvangenBericht();
		Verslag bestaandeVerslag = null;
		if (ontvangenCdaBericht != null)
		{
			bestaandeVerslag = getVerslagVoorSetID(ontvangenCdaBericht, cdaDocument);
			if (bestaandeVerslag != null)
			{
				var ontvangenCdaBerichtBestaandeVerslag = bestaandeVerslag.getOntvangenBericht();
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
		var melding = new MeldingOngeldigCdaBericht();
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
				var parent = instelling.getParent();
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
		for (var id : ids)
		{
			if (id.getRoot() != null && !CdaOID.BSN.equals(id.getRoot()))
			{
				if (id.getExtension() != null && StringUtils.isBlank(id.getExtension()))
				{
					createOngeldigBerichtMelding(cdaDocument, verslag,
						"verslag ongeldig lokaal patientnummer, BSN " + verslag.getScreeningRonde().getDossier().getClient().getPersoon().getBsn(), false);
				}
				var patientnummer = CDAHelper.getRootExtension(id);

				var splittedPatientnummer = patientnummer.split("\\.");
				ArrayUtils.reverse(splittedPatientnummer);
				for (var patientnummerPart : splittedPatientnummer)
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

	private void verwerkCervixCytologieVerslagContent(ClinicalDocument cdaDocument, CervixCytologieVerslag cytologieVerslag)
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
			var deproxiedVerslag = (CervixCytologieVerslag) HibernateHelper.deproxy(verslag);
			getCervixCytologieUitvoerder(cdaDocument, deproxiedVerslag);
		}
		else if (verslag instanceof MammaFollowUpVerslag)
		{
			getFollowUpLaboratorium(cdaDocument, verslag);
		}
	}

	private void getFollowUpLaboratorium(ClinicalDocument cdaDocument, Verslag verslag) throws OngeldigCdaException
	{
		var assignedAuthor = CDAHelper.getAssigendEntity(cdaDocument);

		var organisationId = new StringBuilder();
		getOrganisatie(assignedAuthor, organisationId);

		((MammaVerslag) verslag).setLabCode(organisationId.toString());
	}

	private void getPaLabUitvoerder(ClinicalDocument cdaDocument, Verslag verslag) throws OngeldigCdaException
	{
		var assignedAuthor = CDAHelper.getAssigendEntity(cdaDocument);

		Instelling instelling = null;
		var organisationId = new StringBuilder();
		instelling = getOrganisatie(assignedAuthor, organisationId, OrganisatieType.PA_LABORATORIUM);

		if (instelling == null)
		{
			var message = "verslag ";
			if (organisationId.length() > 0)
			{
				message += "met onbekende PA lab identificatie(s) " + organisationId;
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
			var message = "verslag waarbij een PA lab gevonden met een van de identificatie(s) " + organisationId + " niet gekoppeld is aan een zorginstelling";
			message += " (" + CDAHelper.getFirstValueNotNull(assignedAuthor, CommonCdaConstants.ORGANIZATION_NAME_SUBPATH) + ")";
			createOngeldigBerichtMelding(cdaDocument, verslag, message, true);
		}

		InstellingGebruiker instellingMedewerker = null;

		var patholoogId = getPatholoogId(cdaDocument);
		if (StringUtils.isBlank(patholoogId))
		{
			var message = "verslag zonder geldige patholoog identificatie";
			message += " (" + CDAHelper.getFirstValueNotNull(assignedAuthor, CommonCdaConstants.ORGANIZATION_NAME_SUBPATH) + ")";
			createOngeldigBerichtMelding(cdaDocument, verslag, message, false);
		}
		if (CollectionUtils.isNotEmpty(instelling.getOrganisatieMedewerkers()) && patholoogId != null)
		{
			for (var ig : instelling.getOrganisatieMedewerkers())
			{
				if (Boolean.TRUE.equals(ig.getActief()))
				{
					var medewerker = ig.getMedewerker();
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
			var melding = "verslag ";
			if (StringUtils.isNotBlank(patholoogId))
			{
				melding += "met ongeldige patholoog identificatie " + patholoogId;
			}

			if (assignedAuthor != null)
			{
				var assignedPerson = assignedAuthor.getAssignedPerson();
				if (assignedPerson != null)
				{
					var names = assignedPerson.getNames();
					var namePart = CDAHelper.getNamePart(names, EnPrefix.class, -1, "") + " " + CDAHelper.getNamePart(names, EnFamily.class, -1, "onbekend") + ", "
						+ CDAHelper.getNamePart(names, EnGiven.class, -1, "");
					melding += " (" + namePart.trim() + ")";
				}

			}
			if (StringUtils.isNotBlank(organisationId.toString()))
			{
				melding += " (binnen pathologielaboratorium met identificatie " + organisationId + ")";
			}
			createOngeldigBerichtMelding(cdaDocument, verslag, melding, true);
		}
		else
		{
			var vervanging = getOlderVerslag(verslag, cdaDocument) != null;

			var overeenkomstPeildatum = getReferentieDatum(cdaDocument, CdaConstants.AANVANG_VERRICHTING_DATUM_PA_PATH);
			var hasActiveKwaliteitsovereenkomst = kwaliteitsovereenkomstService.hasActiveKwaliteitsovereenkomst(instellingMedewerker.getMedewerker(), overeenkomstPeildatum);
			if (vervanging || hasActiveKwaliteitsovereenkomst)
			{
				verslag.setUitvoerderOrganisatie(instellingMedewerker.getOrganisatie());
				verslag.setUitvoerderMedewerker(instellingMedewerker.getMedewerker());
			}
			else
			{
				var melding = "Patholoog " + instellingMedewerker.getMedewerker().getNaamVolledig() + " (" + instellingMedewerker.getOrganisatie().getNaam()
					+ ") met identificatie '" + patholoogId + "' heeft geen (actieve) overeenkomst";
				createOngeldigBerichtMelding(cdaDocument, verslag, melding, true);
			}
		}
	}

	private void getMdlUitvoerder(ClinicalDocument cdaDocument, Verslag verslag) throws OngeldigCdaException
	{
		var assignedAuthor = CDAHelper.getAssigendEntity(cdaDocument);

		var organisationId = new StringBuilder();
		var instelling = getOrganisatie(assignedAuthor, organisationId, OrganisatieType.COLOSCOPIELOCATIE, OrganisatieType.ZORGINSTELLING);

		if (instelling == null)
		{
			var message = "verslag ";
			if (organisationId.length() > 0)
			{
				message += "met ongeldige coloscopielocatie/zorginstelling identificatie(s) " + organisationId;
			}
			else
			{
				message += "zonder geldige coloscopielocatie/zorginstelling identificatie";
			}
			message += " (" + CDAHelper.getFirstValueNotNull(assignedAuthor, CommonCdaConstants.ORGANIZATION_NAME_SUBPATH) + ")";
			createOngeldigBerichtMelding(cdaDocument, verslag, message, true);
		}
		Gebruiker instellingMedewerker = null;

		var uzinummer = CDAHelper.getUzinummer(cdaDocument);
		if (StringUtils.isNotBlank(uzinummer))
		{
			if (OrganisatieType.ZORGINSTELLING.equals(instelling.getOrganisatieType()))
			{
				for (var child : instelling.getChildren())
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
			var message = "verslag ";
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
				var assignedPerson = assignedAuthor.getAssignedPerson();
				if (assignedPerson != null)
				{
					var names = assignedPerson.getNames();
					var namePart = CDAHelper.getNamePart(names, EnPrefix.class, -1, "") + " " + CDAHelper.getNamePart(names, EnFamily.class, -1, "onbekend") + ", "
						+ CDAHelper.getNamePart(names, EnGiven.class, -1, "");
					message += " (" + namePart.trim() + ")";
				}

			}
			if (StringUtils.isNotBlank(organisationId.toString()))
			{
				message += " (binnen coloscopielocatie/zorginstelling met identificatie " + organisationId + ")";
			}

			createOngeldigBerichtMelding(cdaDocument, verslag, message, true);
		}
		else
		{
			var vervanging = getOlderVerslag(verslag, cdaDocument) != null;

			var overeenkomstPeildatum = getReferentieDatum(cdaDocument, CdaConstants.AANVANG_VERRICHTING_DATUM_MDL_PATH);
			var hasActiveKwaliteitsovereenkomst = kwaliteitsovereenkomstService.hasActiveKwaliteitsovereenkomst(instellingMedewerker, overeenkomstPeildatum);
			if (vervanging || hasActiveKwaliteitsovereenkomst)
			{
				verslag.setUitvoerderOrganisatie(instelling);
				verslag.setUitvoerderMedewerker(instellingMedewerker);
			}
			else
			{
				var melding = "Medewerker " + instellingMedewerker.getNaamVolledig() + " (" + instelling.getNaam() + ") met uzinummer " + uzinummer
					+ " heeft geen (actieve) overeenkomst";
				createOngeldigBerichtMelding(cdaDocument, verslag, melding, true);
			}
		}
	}

	private void getCervixCytologieUitvoerder(ClinicalDocument cdaDocument, CervixCytologieVerslag verslag) throws OngeldigCdaException
	{
		var assignedAuthor = CDAHelper.getAssigendEntity(cdaDocument);
		var organisationId = new StringBuilder();

		var instelling = getOrganisatie(assignedAuthor, organisationId, OrganisatieType.BMHK_LABORATORIUM);
		if (instelling == null)
		{
			var message = "verslag ";
			boolean herstelbaar;
			if (organisationId.length() > 0)
			{
				message += "met onbekende BMHK laboratorium identificatie(s) " + organisationId;
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

		var patholoogId = getPatholoogId(cdaDocument);
		if (StringUtils.isBlank(patholoogId))
		{
			var message = "verslag zonder geldige patholoog identificatie";
			message += " (" + CDAHelper.getFirstValueNotNull(assignedAuthor, CommonCdaConstants.ORGANIZATION_NAME_SUBPATH) + ")";
			createOngeldigBerichtMelding(cdaDocument, verslag, message, false);
		}

		var patholoogNaam = getPatholoogNaam(cdaDocument);
		if (StringUtils.isBlank(patholoogNaam))
		{
			var message = "verslag zonder geldige patholoog ondertekening";
			createOngeldigBerichtMelding(cdaDocument, verslag, message, false);
		}

		InstellingGebruiker instellingGebruiker = null;
		if (CollectionUtils.isNotEmpty(instelling.getOrganisatieMedewerkers()) && patholoogId != null)
		{
			for (var ig : instelling.getOrganisatieMedewerkers())
			{
				if (Boolean.TRUE.equals(ig.getActief()))
				{
					var medewerker = ig.getMedewerker();
					if (Boolean.TRUE.equals(medewerker.getActief()) && patholoogId.equals(medewerker.getPatholoogId()))
					{
						instellingGebruiker = ig;
						break;
					}
				}
			}
		}
		if (instellingGebruiker == null)
		{
			var melding = "verslag ";
			if (StringUtils.isNotBlank(patholoogId))
			{
				melding += "met ongeldige patholoog identificatie " + patholoogId;
			}

			if (assignedAuthor != null)
			{
				var assignedPerson = assignedAuthor.getAssignedPerson();
				if (assignedPerson != null)
				{
					var names = assignedPerson.getNames();
					var namePart = CDAHelper.getNamePart(names, EnPrefix.class, -1, "") + " " + CDAHelper.getNamePart(names, EnFamily.class, -1, "onbekend") + ", "
						+ CDAHelper.getNamePart(names, EnGiven.class, -1, "");
					melding += " (" + namePart.trim() + ")";
				}
			}

			if (StringUtils.isNotBlank(organisationId.toString()))
			{
				melding += " (binnen BMHK laboratorium met identificatie " + organisationId + ")";
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
		var assignedAuthor = CDAHelper.getAssigendAuthor(cdaDocument);
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
			onderzoekDatum = CDAHelper.converCdaDateStringToDate(CDAHelper.getFirstValue(cdaDocument, onderzoekdatumPath));
		}
		catch (ParseException e)
		{
			LOG.error("Fout bij ophalen referentiedatum {}", e.getMessage());
		}
		return onderzoekDatum;
	}

	private boolean isPabLabGekoppeldAanZorginstelling(Instelling instelling)
	{
		PaLaboratorium pa = null;
		var gekoppeld = false;
		if (OrganisatieType.PA_LABORATORIUM.equals(instelling.getOrganisatieType()))
		{
			pa = (PaLaboratorium) instelling;
		}
		if (pa != null && pa.getColoscopielocaties().size() > 0)
		{
			for (var col : pa.getColoscopielocaties())
			{
				var zorginstelling = col.getParent();
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
			for (var ig : instelling.getOrganisatieMedewerkers())
			{
				var medewerker = ig.getMedewerker();
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
			var representedOrganization = assignedAuthor.getRepresentedOrganization();
			if (representedOrganization != null && representedOrganization.getIds() != null)
			{
				for (var ii : representedOrganization.getIds())
				{
					Instelling foundInstelling = null;
					if (ii.getRoot() != null)
					{
						var root = ii.getRoot();
						var extension = ii.getExtension();
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
			var zorgInstelling = (ZorgInstelling) instelling;
			ColoscopieLocatie locatie = null;
			for (var child : zorgInstelling.getChildren())
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
		var assignedAuthor = CDAHelper.getAssigendEntity(cda);
		var organisationId = new StringBuilder();

		return getOrganisatie(assignedAuthor, organisationId, types);
	}

	private String getPaLabId(POCDMT000040AssignedEntity assignedAuthor)
	{
		String paLabId = null;
		if (assignedAuthor != null)
		{
			var representedOrganization = assignedAuthor.getRepresentedOrganization();
			if (representedOrganization != null)
			{
				for (var ii : representedOrganization.getIds())
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
			var uzinummer = CDAHelper.getUzinummer(cda);

			if (StringUtils.isNotBlank(uzinummer))
			{
				medewerker = gebruikersService.getGebruikerByUzinummer(uzinummer).orElse(null);
			}
			break;
		case PA_LAB:
		case CERVIX_CYTOLOGIE:
			var patholoogId = getPatholoogId(cda);

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
		var assignedAuthor = CDAHelper.getAssigendEntity(cda);
		String patholoogId = null;
		if (assignedAuthor != null)
		{
			var paLabId = getPaLabId(assignedAuthor);
			if (StringUtils.isNotBlank(paLabId))
			{
				patholoogId = CDAHelper.getExtension(paLabId + ".1", assignedAuthor.getIds());
			}
		}
		return patholoogId;
	}

}
