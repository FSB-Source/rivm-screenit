package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.io.IOException;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.CervixHL7BaseService;
import nl.rivm.screenit.batch.service.CervixOrderBerichtService;
import nl.rivm.screenit.batch.service.HL7BaseSendMessageService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.cervix.CervixHpvAnalyseresultaten;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultValue;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieReden;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.databind.ObjectMapper;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.model.DataTypeException;
import ca.uhn.hl7v2.model.v24.datatype.FT;
import ca.uhn.hl7v2.model.v24.datatype.XCN;
import ca.uhn.hl7v2.model.v24.group.OML_O21_CONTAINER_2;
import ca.uhn.hl7v2.model.v24.group.OML_O21_OBSERVATION_REQUEST;
import ca.uhn.hl7v2.model.v24.group.OML_O21_ORDER;
import ca.uhn.hl7v2.model.v24.group.OML_O21_ORDER_GENERAL;
import ca.uhn.hl7v2.model.v24.group.OML_O21_PATIENT;
import ca.uhn.hl7v2.model.v24.group.OML_O21_PATIENT_VISIT;
import ca.uhn.hl7v2.model.v24.message.OML_O21;
import ca.uhn.hl7v2.model.v24.segment.MSH;
import ca.uhn.hl7v2.model.v24.segment.OBR;
import ca.uhn.hl7v2.model.v24.segment.OBX;
import ca.uhn.hl7v2.model.v24.segment.ORC;
import ca.uhn.hl7v2.model.v24.segment.PV1;

@Slf4j
@AllArgsConstructor
@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CervixOrderBerichtServiceImpl implements CervixOrderBerichtService
{

	private static final String DATUM_UITSTRIJKJE = "CR_DATUMSTRIJK";

	private static final String DATUM_AFNAME = "CR_DATUMAFNAME";

	private static final String AANLEIDING = "CR_AANLEIDING";

	private static final String AANLEIDINGHPV = "CR_AANLEIDINGHPV";

	private static final String DOOR = "CR_DOOR";

	private static final String KLACHTEN = "CR_KLACHTEN";

	private static final String KLACHTEN_ANDERS = "CR_KLACHTENANDERS";

	private static final String LAATSTE_MENSTRUATIE = "CR_DATUMLM";

	private static final String PATROON = "CR_PATROON";

	private static final String ANTICONCEPTIE = "CR_ANTICONCEPTIE";

	private static final String HORMOONGEBRUIK = "CR_HORMOONGEBRUIK";

	private static final String HORMOONGEBRUIK_ANDERS = "CR_HORMOONGEBRUIKANDERS";

	private static final String ASPECT = "CR_ASPECT";

	private static final String ASPECT_AFWIJKING_PORTIO = "CR_AFWIJKINGPORTIO";

	private static final String OPMERKINGEN = "CR_OPMERKING";

	private static final String VERSIE = "CR_VERSIE";

	private static final String BVOREGNR = "CR_BVOREGNR";

	private static final String HPVTRIAGEBVO = "CR_HPVTRIAGEBVO";

	private final CervixHL7BaseService hl7BaseService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final OrganisatieParameterService organisatieParameterService;

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier dateSupplier;

	private final HibernateService hibernateService;

	private final HL7BaseSendMessageService sendMessageService;

	private final ObjectMapper objectMapper = new ObjectMapper();

	private static void vulHuisartsRedords(CervixLabformulier labformulier, XCN berichtHuisarts) throws DataTypeException
	{
		CervixHuisarts huisarts = labformulier.getHuisartsLocatie().getHuisarts();
		Gebruiker arts = huisarts.getOrganisatieMedewerkers().get(0).getMedewerker();
		berichtHuisarts.getGivenName().setValue(arts.getVoorletters());
		berichtHuisarts.getXcn1_IDNumber().setValue(huisarts.getAgbcode());
		berichtHuisarts.getIdentifierCheckDigit().setValue(huisarts.getAgbcode());
		berichtHuisarts.getFamilyName().getSurname().setValue(arts.getAchternaam());
		berichtHuisarts.getFamilyName().getOwnSurnamePrefix().setValue(arts.getTussenvoegsel());
		berichtHuisarts.getFamilyName().getFn2_OwnSurnamePrefix().setValue(arts.getVoorletters());
	}

	@Override
	public String maakCytologieOrderTextBericht(CervixUitstrijkje uitstrijkje, CervixCytologieReden cytologieReden)
	{
		Client client = uitstrijkje.getOntvangstScreeningRonde().getDossier().getClient();
		CervixLabformulier labformulier = uitstrijkje.getLabformulier();

		OML_O21 omlBericht = new OML_O21();

		try
		{
			omlBericht.initQuickstart("OML", "O21", "P");

			vulHeaderVoorCytoOrder(omlBericht);

			vulPatientRecords(client, omlBericht);

			vulHuisartsRecordsVoorCytoOrder(labformulier, omlBericht);

			vulCytologieOrderRecords(cytologieReden, labformulier, omlBericht);

		}
		catch (HL7Exception | IOException | IllegalStateException e)
		{
			LOG.error("Er is een fout opgetreden bij het maken van een order!", e);
		}
		return omlBericht.toString();
	}

	@Override
	public String maakHpvOrderTextBericht(CervixMonster monster, boolean cancelOrder)
	{
		Client client = monster.getUitnodiging().getScreeningRonde().getDossier().getClient();

		OML_O21 omlBericht = new OML_O21();

		try
		{
			omlBericht.initQuickstart("OML", "O21", "P");

			vulHeaderVoorHpvOrder(omlBericht);

			CervixLabformulier labformulier = getLabformulier(monster);
			vulHuisartsRecordsVoorHpvOrder(labformulier, omlBericht);

			vulPatientRecords(client, omlBericht);

			vulHpvOrderRecords(monster, omlBericht, cancelOrder);

		}
		catch (HL7Exception | IOException | IllegalStateException e)
		{
			LOG.error("Er is een fout opgetreden bij het maken van een order!", e);
		}
		return omlBericht.toString();
	}

	private void vulHeaderVoorCytoOrder(OML_O21 oml_bericht) throws DataTypeException
	{
		MSH mshSegment = vulHeader(oml_bericht);
		mshSegment.getReceivingFacility().getNamespaceID().setValue("PALGA");
		mshSegment.getReceivingApplication().getNamespaceID().setValue("UDPS");
	}

	private void vulHeaderVoorHpvOrder(OML_O21 oml_bericht) throws DataTypeException
	{
		MSH mshSegment = vulHeader(oml_bericht);
		mshSegment.getReceivingFacility().getNamespaceID().setValue("LIMS");
		mshSegment.getReceivingApplication().getNamespaceID().setValue("LIMS");
	}

	private MSH vulHeader(OML_O21 oml_bericht) throws DataTypeException
	{
		MSH mshSegment = oml_bericht.getMSH();
		hl7BaseService.buildMessageHeader(mshSegment, "SCREENIT_OML");
		mshSegment.getPrincipalLanguageOfMessage().getText().setValue("NLD");
		mshSegment.getAlternateCharacterSetHandlingScheme().setValue("AL");
		mshSegment.getDateTimeOfMessage().getTimeOfAnEvent().setValue(getCurrentDateTimeString());
		return mshSegment;
	}

	private void vulPatientRecords(Client client, OML_O21 omlBericht) throws DataTypeException
	{
		OML_O21_PATIENT omlPatient = omlBericht.getPATIENT();

		hl7BaseService.buildPIDSegmentWithForcedGender(omlPatient.getPID(), client, Geslacht.VROUW);

	}

	private void vulHuisartsRecordsVoorCytoOrder(CervixLabformulier labformulier, OML_O21 omlBericht) throws DataTypeException
	{
		if (labformulier != null && labformulier.getHuisartsLocatie() != null)
		{
			OML_O21_PATIENT_VISIT visit = omlBericht.getPATIENT().getPATIENT_VISIT();
			PV1 pv1 = visit.getPV1();
			pv1.getPv11_SetIDPV1().setValue("1");
			pv1.getPatientClass().setValue("O");

			XCN berichtHuisarts = pv1.getPv18_ReferringDoctor(0);
			vulHuisartsRedords(labformulier, berichtHuisarts);
		}
	}

	private void vulHuisartsRecordsVoorHpvOrder(CervixLabformulier labformulier, OML_O21 omlBericht) throws DataTypeException
	{
		if (labformulier != null && labformulier.getHuisartsLocatie() != null)
		{
			ORC orc = omlBericht.getORDER_GENERAL().getORDER().getORC();
			XCN berichtHuisarts = orc.getOrc12_OrderingProvider(0);
			vulHuisartsRedords(labformulier, berichtHuisarts);
		}
	}

	private void vulCytologieOrderRecords(CervixCytologieReden cytologieReden, CervixLabformulier labformulier, OML_O21 omlBericht) throws HL7Exception, IllegalStateException
	{
		OML_O21_ORDER_GENERAL order_general = omlBericht.getORDER_GENERAL();

		OML_O21_ORDER orderSegment = order_general.getORDER();
		ORC orc = orderSegment.getORC();
		orc.getOrc1_OrderControl().setValue("NW");
		orc.getOrc2_PlacerOrderNumber().getEi1_EntityIdentifier().setValue(CervixMonsterUtil.getMonsterEntityIdentifier(labformulier.getUitstrijkje(), false));
		orc.getOrc2_PlacerOrderNumber().getEi2_NamespaceID().setValue("ScreenIT");
		orc.getDateTimeOfTransaction().getTs1_TimeOfAnEvent().setValue(getCurrentDateTimeString());

		OML_O21_OBSERVATION_REQUEST observationRequest = orderSegment.getOBSERVATION_REQUEST();
		OBR obr = hl7BaseService.buildOBRSegment(observationRequest.getOBR(), labformulier.getUitstrijkje(), false);
		obr.getRequestedDateTime().getTs1_TimeOfAnEvent().setValue(hl7BaseService.getCurrentDateTimeString());
		obr.getObr4_UniversalServiceIdentifier().getCe1_Identifier().setValue("Cervixcytologie");

		Date datumUitstrijkje = labformulier.getDatumUitstrijkje();
		if (datumUitstrijkje != null)
		{
			makeNewObxRecord(omlBericht, DATUM_UITSTRIJKJE, DateUtil.formatForPattern(Constants.DEFAULT_DATE_TIME_FORMAT_SHORT_YEAR, datumUitstrijkje));
		}
		makeNewObxRecord(omlBericht, AANLEIDING, cytologieReden.getOmlOrderCode());
		makeNewObxRecord(omlBericht, DOOR, "1");
		vulOBXRegelsMetKlachten(omlBericht, labformulier);
		Date datumLaatsteMenstruatie = labformulier.getDatumLaatsteMenstruatie();
		if (datumLaatsteMenstruatie != null)
		{
			makeNewObxRecord(omlBericht, LAATSTE_MENSTRUATIE, DateUtil.formatForPattern(Constants.DEFAULT_DATE_TIME_FORMAT_SHORT_YEAR, datumLaatsteMenstruatie));
		}
		vulOBXRegelsMetPatroon(omlBericht, labformulier);
		vulOBXRegelsMetAnticonceptie(omlBericht, labformulier);
		vulOBXRegelsMetHomoonGebruik(omlBericht, labformulier);
		vulOBXRegelsMetAspect(omlBericht, labformulier);
		vulOBXRegelMetOpmerkingen(omlBericht, labformulier);
		vulOBXRegelsMetVersie(omlBericht);
		vulOBXRegelsMetBvoRegNr(omlBericht, labformulier);
		var startDatumGenotypering =
			DateUtil.parseLocalDateForPattern(preferenceService.getString(PreferenceKey.CERVIX_START_AANLEVERING_GENOTYPERING_EN_INVOERING_TRIAGE.name()), "yyyyMMdd");
		if (startDatumGenotypering != null
			&& !dateSupplier.getLocalDate().isBefore(startDatumGenotypering)
			&& organisatieParameterService.getOrganisatieParameter(labformulier.getLaboratorium(), OrganisatieParameterKey.CERVIX_ORDER_NIEUWE_STIJL, Boolean.FALSE))
		{
			vulOBXRegelsMetHpvtriagebvo(omlBericht, labformulier);
		}
	}

	private void vulHpvOrderRecords(CervixMonster monster, OML_O21 omlBericht, boolean cancelOrder) throws HL7Exception, IllegalStateException
	{
		var orderGeneral = omlBericht.getORDER_GENERAL();

		var orderSegment = orderGeneral.getORDER();
		var orc = orderSegment.getORC();
		orc.getOrc1_OrderControl().setValue(cancelOrder ? "CA" : "NW");
		orc.getOrc2_PlacerOrderNumber().getEi1_EntityIdentifier().setValue(CervixMonsterUtil.getMonsterEntityIdentifier(monster, true));
		orc.getOrc2_PlacerOrderNumber().getEi2_NamespaceID().setValue("ScreenIT");
		orc.getDateTimeOfTransaction().getTs1_TimeOfAnEvent().setValue(getCurrentDateTimeString());
		orc.getOrc4_PlacerGroupNumber().getEi1_EntityIdentifier().setValue(CervixMonsterUtil.getMonsterEntityIdentifier(monster, false));

		var observationRequest = orderSegment.getOBSERVATION_REQUEST();
		var obr = hl7BaseService.buildOBRSegment(observationRequest.getOBR(), monster, true);
		obr.getObr4_UniversalServiceIdentifier().getCe1_Identifier().setValue("HPV");
		obr.getObr31_ReasonForStudy(0).getCe1_Identifier().setValue("BVO-BMHK");

		makeNewObxRecord(omlBericht, DATUM_AFNAME, DateUtil.formatForPattern(Constants.DEFAULT_DATE_TIME_FORMAT_SHORT_YEAR, getDatumAfname(monster)));
		makeNewObxRecord(omlBericht, AANLEIDINGHPV, (CervixMonsterUtil.isUitstrijkje(monster) ? "0" : "1"));
		makeNewObxRecord(omlBericht, DOOR, (CervixMonsterUtil.isUitstrijkje(monster) ? "1" : "0"));
	}

	private void vulOBXRegelsMetKlachten(OML_O21 omlBericht, CervixLabformulier labformulier) throws HL7Exception
	{
		if (labformulier.isKlachtenGeen())
		{
			makeNewObxRecord(omlBericht, KLACHTEN, "0");
		}
		if (labformulier.isKlachtenContactbloedingen())
		{
			makeNewObxRecord(omlBericht, KLACHTEN, "2");
		}
		if (labformulier.isKlachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak())
		{
			makeNewObxRecord(omlBericht, KLACHTEN, "3");
		}
		if (labformulier.isKlachtenIntermenstrueelBloedverlies())
		{
			makeNewObxRecord(omlBericht, KLACHTEN, "4");
		}
		if (labformulier.isKlachtenPostmenopauzaalBloedverlies())
		{
			makeNewObxRecord(omlBericht, KLACHTEN, "5");
		}
		if (labformulier.isKlachtenAndersNamelijk())
		{
			makeNewObxRecord(omlBericht, KLACHTEN, "9");
		}
		if (StringUtils.isNotBlank(labformulier.getKlachtenAndersNamelijkTekst()))
		{
			makeNewObxRecord(omlBericht, KLACHTEN_ANDERS, labformulier.getKlachtenAndersNamelijkTekst());
		}
	}

	private void vulOBXRegelsMetVersie(OML_O21 omlBericht) throws HL7Exception
	{
		makeNewObxRecord(omlBericht, VERSIE, "4");
	}

	private void vulOBXRegelsMetBvoRegNr(OML_O21 omlBericht, CervixLabformulier labformulier) throws HL7Exception
	{
		makeNewObxRecord(omlBericht, BVOREGNR, CervixMonsterUtil.getMonsterEntityIdentifier(labformulier.getUitstrijkje(), false));
	}

	private void vulOBXRegelsMetPatroon(OML_O21 omlBericht, CervixLabformulier labformulier) throws HL7Exception
	{
		if (labformulier.isMenstruatieNormaal())
		{
			makeNewObxRecord(omlBericht, PATROON, "0");
		}
		if (labformulier.isMenstruatieMenopauze())
		{
			makeNewObxRecord(omlBericht, PATROON, "3");
		}
		if (labformulier.isMenstruatiePostmenopauze())
		{
			makeNewObxRecord(omlBericht, PATROON, "4");
		}
		if (labformulier.isMenstruatieGeenMenstruatie())
		{
			makeNewObxRecord(omlBericht, PATROON, "5");
		}
	}

	private void vulOBXRegelsMetAnticonceptie(OML_O21 omlBericht, CervixLabformulier labformulier) throws HL7Exception
	{
		if (labformulier.isAnticonceptieGeen())
		{
			makeNewObxRecord(omlBericht, ANTICONCEPTIE, "0");
		}
		if (labformulier.isAnticonceptiePil())
		{
			makeNewObxRecord(omlBericht, ANTICONCEPTIE, "1");
		}
		if (labformulier.isAnticonceptieIudKoper())
		{
			makeNewObxRecord(omlBericht, ANTICONCEPTIE, "3");
		}
		if (labformulier.isAnticonceptieIudMirena())
		{
			makeNewObxRecord(omlBericht, ANTICONCEPTIE, "4");
		}
		if (labformulier.isAnticonceptieAnders())
		{
			makeNewObxRecord(omlBericht, ANTICONCEPTIE, "9");
		}
	}

	private void vulOBXRegelsMetHomoonGebruik(OML_O21 omlbericht, CervixLabformulier labformulier) throws HL7Exception
	{
		if (labformulier.isGebruikHormonenGeen())
		{
			makeNewObxRecord(omlbericht, HORMOONGEBRUIK, "0");
		}
		if (labformulier.isGebruikHormonenJaVanwegeOvergangsklachten())
		{
			makeNewObxRecord(omlbericht, HORMOONGEBRUIK, "2");
		}
		if (labformulier.isGebruikHormonenJaVanwegeBorstkanker())
		{
			makeNewObxRecord(omlbericht, HORMOONGEBRUIK, "3");
		}
		if (labformulier.isGebruikHormonenJaVanwege())
		{
			makeNewObxRecord(omlbericht, HORMOONGEBRUIK, "9");
		}
		if (StringUtils.isNotBlank(labformulier.getGebruikHormonenJaVanwegeTekst()))
		{
			makeNewObxRecord(omlbericht, HORMOONGEBRUIK_ANDERS, labformulier.getGebruikHormonenJaVanwegeTekst());
		}
	}

	private void vulOBXRegelsMetAspect(OML_O21 omlbericht, CervixLabformulier labformulier) throws HL7Exception
	{
		if (labformulier.isAspectCervixNormaal())
		{
			makeNewObxRecord(omlbericht, ASPECT, "1");
		}
		if (labformulier.isAspectCervixNietGezien())
		{
			makeNewObxRecord(omlbericht, ASPECT, "2");
		}
		if (labformulier.isAspectCervixAbnormaalOfVerdachtePortio())
		{
			makeNewObxRecord(omlbericht, ASPECT, "3");
		}
		if (StringUtils.isNotBlank(labformulier.getAspectCervixAbnormaalOfVerdachtePortioTekst()))
		{
			makeNewObxRecord(omlbericht, ASPECT_AFWIJKING_PORTIO, labformulier.getAspectCervixAbnormaalOfVerdachtePortioTekst());
		}
	}

	private void vulOBXRegelMetOpmerkingen(OML_O21 omlBericht, CervixLabformulier labformulier) throws HL7Exception, IllegalStateException
	{

		var startDatumGenotypering =
			DateUtil.parseLocalDateForPattern(preferenceService.getString(PreferenceKey.CERVIX_START_AANLEVERING_GENOTYPERING_EN_INVOERING_TRIAGE.name()), "yyyyMMdd");
		var analyseresultaten = getAnalyseresultaten(labformulier);
		if (startDatumGenotypering != null
			&& !dateSupplier.getLocalDate().isBefore(startDatumGenotypering)
			&& !organisatieParameterService.getOrganisatieParameter(labformulier.getLaboratorium(), OrganisatieParameterKey.CERVIX_ORDER_NIEUWE_STIJL, Boolean.FALSE)
			&& analyseresultaten != null
			&& analyseresultaten.getHpv16() != null)
		{
			vulOpmerkingenVeldMetGenotyperingenVoorLabsOudeStijl(omlBericht, labformulier);
			return;
		}

		if (labformulier.isOpmerkingen())
		{
			makeNewObxRecord(omlBericht, OPMERKINGEN, "1");
		}
		else
		{
			makeNewObxRecord(omlBericht, OPMERKINGEN, "0");
		}
		if (StringUtils.isNotBlank(labformulier.getOpmerkingenTekst()))
		{
			makeNewObxRecord(omlBericht, OPMERKINGEN + 1, labformulier.getOpmerkingenTekst());
		}
	}

	private void vulOpmerkingenVeldMetGenotyperingenVoorLabsOudeStijl(OML_O21 omlBericht, CervixLabformulier labformulier) throws HL7Exception, IllegalStateException
	{
		var analyseresultaten = getAnalyseresultaten(labformulier);
		makeNewObxRecord(omlBericht, OPMERKINGEN, "1");
		var opmerkingenTekst = labformulier.getOpmerkingenTekst();
		if (labformulier.isOpmerkingen()
			&& StringUtils.isBlank(opmerkingenTekst))
		{
			opmerkingenTekst = "Zie ScreenIT";
		}
		else if (!labformulier.isOpmerkingen())
		{
			opmerkingenTekst = "";
		}
		makeNewObxRecord(omlBericht, OPMERKINGEN + 1,
			String.format("GENOTYPERING: %s %s %s, opmerkingen: %s",
				analyseresultaten.getHpv16(),
				analyseresultaten.getHpv18(),
				analyseresultaten.getHpvohr(),
				opmerkingenTekst));
	}

	private void vulOBXRegelsMetHpvtriagebvo(OML_O21 omlBericht, CervixLabformulier labformulier) throws HL7Exception
	{
		var analyseresultaten = getAnalyseresultaten(labformulier);
		if (analyseresultaten != null)
		{
			if (CervixHpvResultValue.POS_HPV16.equals(analyseresultaten.getHpv16()))
			{
				makeNewObxRecord(omlBericht, HPVTRIAGEBVO, "1");
			}
			if (CervixHpvResultValue.POS_HPV18.equals(analyseresultaten.getHpv18()))
			{
				makeNewObxRecord(omlBericht, HPVTRIAGEBVO, "2");
			}
			if (CervixHpvResultValue.POS_OTHER_HR_HPV.equals(analyseresultaten.getHpvohr()))
			{
				makeNewObxRecord(omlBericht, HPVTRIAGEBVO, "9");
			}
		}
	}

	private CervixHpvAnalyseresultaten getAnalyseresultaten(CervixLabformulier labformulier)
	{
		if (labformulier.getUitstrijkje() != null
			&& labformulier.getUitstrijkje().getOntvangstScreeningRonde() != null
			&& labformulier.getUitstrijkje().getOntvangstScreeningRonde().getMonsterHpvUitslag() != null
			&& labformulier.getUitstrijkje().getOntvangstScreeningRonde().getMonsterHpvUitslag().getLaatsteHpvBeoordeling() != null
			&& labformulier.getUitstrijkje().getOntvangstScreeningRonde().getMonsterHpvUitslag().getLaatsteHpvBeoordeling().getAnalyseresultaten() != null)
		{
			return labformulier.getUitstrijkje()
				.getOntvangstScreeningRonde()
				.getMonsterHpvUitslag()
				.getLaatsteHpvBeoordeling()
				.getAnalyseresultaten();
		}
		return null;
	}

	private void makeNewObxRecord(OML_O21 message, String identifier, String value) throws HL7Exception
	{
		OML_O21_ORDER_GENERAL order_general = message.getORDER_GENERAL();
		OML_O21_ORDER orderSegment = order_general.getORDER();
		OML_O21_OBSERVATION_REQUEST observationRequest = orderSegment.getOBSERVATION_REQUEST();
		OML_O21_CONTAINER_2 container = observationRequest.getCONTAINER_2();

		List<OBX> obxen = container.getOBXAll();
		int newObxNmr = obxen.size();
		OBX newOBX = container.getOBX(newObxNmr);
		newOBX.getObx1_SetIDOBX().setValue(String.valueOf(newObxNmr));
		newOBX.getObx2_ValueType().setValue("FT");
		newOBX.getObx3_ObservationIdentifier().getCe1_Identifier().setValue(identifier);

		FT ft = new FT(message);
		ft.setValue(value.replace("\r\n", "*").replace("\r", "*").replace("\n", "*"));
		newOBX.getObx5_ObservationValue(0).setData(ft);
	}

	private String getCurrentDateTimeString()
	{
		return currentDateSupplier.getLocalDateTime().format(DateTimeFormatter.ofPattern(Constants.DATE_FORMAT_YYYYMMDDHHMMSS));
	}

	private CervixLabformulier getLabformulier(CervixMonster monster)
	{
		if (CervixMonsterUtil.isUitstrijkje(monster))
		{
			return CervixMonsterUtil.getUitstrijkje(monster).getLabformulier();
		}
		return null;
	}

	private Date getDatumAfname(CervixMonster monster)
	{
		CervixLabformulier labformulier = getLabformulier(monster);
		if (labformulier != null && labformulier.getDatumUitstrijkje() != null)
		{
			return labformulier.getDatumUitstrijkje();
		}
		else if (monster.getOntvangstdatum() != null)
		{
			return monster.getOntvangstdatum();
		}
		return currentDateSupplier.getDate();
	}

}
