package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Date;

import nl.rivm.screenit.batch.service.MammaHL7CreateMessageService;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerUploadBeeldenDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.HL7Util;
import nl.rivm.screenit.util.NaamUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.model.DataTypeException;
import ca.uhn.hl7v2.model.v24.datatype.CX;
import ca.uhn.hl7v2.model.v24.datatype.XPN;
import ca.uhn.hl7v2.model.v24.message.ADT_AXX;
import ca.uhn.hl7v2.model.v24.message.ORM_O01;
import ca.uhn.hl7v2.model.v24.segment.MRG;
import ca.uhn.hl7v2.model.v24.segment.MSH;
import ca.uhn.hl7v2.model.v24.segment.OBR;
import ca.uhn.hl7v2.model.v24.segment.ORC;
import ca.uhn.hl7v2.model.v24.segment.PID;
import ca.uhn.hl7v2.util.Terser;

@Service
public class MammaHL7CreateMessageServiceImpl implements MammaHL7CreateMessageService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaHL7CreateMessageServiceImpl.class);

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private DateTimeFormatter hl7DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");

	private DateTimeFormatter hl7DateFormatter = DateTimeFormatter.ofPattern("yyyyMMdd");

	@Override
	public ORM_O01 maakOrmIlmBericht(MammaHL7v24ORMBerichtStatus status, Client client, MammaScreeningRonde ronde) throws IOException, HL7Exception
	{
		ORM_O01 ormBericht = createOrm_o01();
		buildPIDSegment(ormBericht.getPATIENT().getPID(), client);
		buildIlmOBRSegment(ormBericht.getORDER().getORDER_DETAIL().getOBR(), client, ronde, status);

		buildORCSegmentILM(ormBericht.getORDER().getORC(), ronde);
		return ormBericht;
	}

	@Override
	public ORM_O01 maakClientORMBericht(MammaHL7v24ORMBerichtStatus status, Client client) throws IOException, HL7Exception
	{
		ORM_O01 ormBericht = createBaseOrmMessage(status, client);
		buildOBRSegment(ormBericht.getORDER().getORDER_DETAIL().getOBR(), client.getMammaDossier().getLaatsteScreeningRonde(), status);
		buildORCSegment(ormBericht.getORDER().getORC(), getAccessionNumber(client.getMammaDossier().getLaatsteScreeningRonde()));
		return ormBericht;
	}

	@Override
	public ORM_O01 maakUploadBeeldenORMBericht(MammaHL7v24ORMBerichtStatus status, MammaHL7v24OrmBerichtTriggerUploadBeeldenDto hl7BerichtTrigger, Client client)
		throws IOException, HL7Exception
	{
		ORM_O01 ormBericht = createBaseOrmMessage(status, client);
		buildUploadVerzoekOBRSegment(ormBericht.getORDER().getORDER_DETAIL().getOBR(), hl7BerichtTrigger);
		buildORCSegment(ormBericht.getORDER().getORC(), hl7BerichtTrigger.getAccessionNumber().toString());
		return ormBericht;
	}

	@Override
	public ORM_O01 maakUploadBeeldenORMILMBericht(MammaHL7v24ORMBerichtStatus status, MammaHL7v24OrmBerichtTriggerUploadBeeldenDto hl7BerichtTrigger, Client client)
		throws IOException, HL7Exception
	{
		ORM_O01 ormBericht = createBaseOrmMessage(status, client);
		buildUploadVerzoekOBRSegment(ormBericht.getORDER().getORDER_DETAIL().getOBR(), hl7BerichtTrigger);
		buildORCSegmentILM(ormBericht.getORDER().getORC(), hl7BerichtTrigger.getAccessionNumber());
		return ormBericht;
	}

	@Override
	public ORM_O01 maakKwaliteitsopnameORMBericht(MammaHL7v24ORMBerichtStatus status, MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto hl7BerichtTrigger)
		throws IOException, HL7Exception
	{
		if (hl7BerichtTrigger.getAccessionNumber().length() > 16)
		{
			LOG.error("Accessionnumber " + hl7BerichtTrigger.getAccessionNumber() + " is langer dan 16 karakters");
		}
		ORM_O01 ormBericht = createOrm_o01();
		buildKwaliteitsopnamePIDSegment(ormBericht.getPATIENT().getPID(), hl7BerichtTrigger);
		buildKwaliteitsopnameORCSegment(ormBericht.getORDER().getORC(), hl7BerichtTrigger);
		buildKwaliteitsopnameOBRSegment(ormBericht.getORDER().getORDER_DETAIL().getOBR(), hl7BerichtTrigger, status);
		return ormBericht;
	}

	private ORM_O01 createOrm_o01() throws HL7Exception, IOException
	{
		ORM_O01 ormBericht = new ORM_O01();
		ormBericht.initQuickstart("ORM", "O01", "P");
		buildMessageHeader(ormBericht.getMSH());
		return ormBericht;
	}

	private ORM_O01 createBaseOrmMessage(MammaHL7v24ORMBerichtStatus status, Client client) throws HL7Exception, IOException
	{
		ORM_O01 ormBericht = createOrm_o01();
		buildPIDSegment(ormBericht.getPATIENT().getPID(), client);
		return ormBericht;
	}

	@Override
	public ADT_AXX maakADTBerichtPersoonsgegevensGewijzigd(Client client) throws IOException, HL7Exception
	{
		ADT_AXX adtBericht = new ADT_AXX();
		adtBericht.initQuickstart("ADT", "A08", "P");
		buildMessageHeader(adtBericht.getMSH());
		buildPIDSegment(adtBericht.getPID(), client);
		return adtBericht;
	}

	@Override
	public ADT_AXX maakADTBerichtGewijzigdBsn(Client client, String oudBsn, String nieuweBsn) throws IOException, HL7Exception
	{
		ADT_AXX adtBericht = new ADT_AXX();
		adtBericht.initQuickstart("ADT", "A40", "P");

		Terser t = new Terser(adtBericht);
		t.set("/MSH-9-3", "ADT_A40");
		buildMessageHeader(adtBericht.getMSH());
		buildPIDSegment(adtBericht.getPATIENT().getPID(), client, nieuweBsn);
		buildMergeSegment(adtBericht.getPATIENT().getMRG(), oudBsn);
		return adtBericht;
	}

	private void buildMessageHeader(MSH mshSegment) throws HL7Exception
	{
		mshSegment.insertCharacterSet(0).setValue("8859/1");
		mshSegment.getSendingApplication().getNamespaceID().setValue("SCREENIT_MAMMA");
		mshSegment.getSendingFacility().getNamespaceID().setValue("SCREENIT");
		mshSegment.getReceivingApplication().getNamespaceID().setValue("IMS");
		mshSegment.getReceivingFacility().getNamespaceID().setValue("SECTRA");
		mshSegment.getDateTimeOfMessage().getTimeOfAnEvent().setValue(hl7DateTimeFormatter.format(currentDateSupplier.getLocalDateTime()));

	}

	private void buildMergeSegment(MRG mrg, String bsn) throws DataTypeException
	{
		CX cx = mrg.getMrg1_PriorPatientIdentifierList(0);
		cx.getCx1_ID().setValue(bsn);
		cx.getCx4_AssigningAuthority().getHd1_NamespaceID().setValue("NLMINBIZA");
		cx.getCx5_IdentifierTypeCode().setValue("NNNLD");
	}

	private void buildPIDSegment(PID pid, Client client, String nieuweBsn) throws DataTypeException
	{
		GbaPersoon persoon = client.getPersoon();

		pid.getSetIDPID().setValue("1");

		pid.getPid2_PatientID();

		pid.getPid3_PatientIdentifierList(0).getCx1_ID().setValue(nieuweBsn);
		pid.getPid3_PatientIdentifierList(0).getCx4_AssigningAuthority().getHd1_NamespaceID().setValue("NLMINBIZA");
		pid.getPid3_PatientIdentifierList(0).getCx5_IdentifierTypeCode().setValue("NNNLD");

		XPN patientGegevens = pid.getPid5_PatientName(0);
		patientGegevens.getGivenName().setValue(NaamUtil.getVoorlettersClient(client));
		patientGegevens.getFamilyName().getSurname().setValue(NaamUtil.getTussenvoegselEnEigenAchternaam(client.getPersoon()));
		patientGegevens.getFamilyName().getOwnSurname().setValue(persoon.getAchternaam());
		patientGegevens.getFamilyName().getOwnSurnamePrefix().setValue(persoon.getTussenvoegsel());

		pid.getPid7_DateTimeOfBirth().getTimeOfAnEvent().setValue(hl7DateFormatter.format(DateUtil.toLocalDate(persoon.getGeboortedatum())));

		if (persoon.getGeslacht() != null)
		{
			pid.getAdministrativeSex().setValue(HL7Util.getGeslachtFormat(persoon.getGeslacht()));
		}

		if (persoon.getOverlijdensdatum() != null)
		{
			pid.getPatientDeathDateAndTime().getTimeOfAnEvent().setValue(hl7DateFormatter.format(DateUtil.toLocalDate(persoon.getOverlijdensdatum())));
			pid.getPatientDeathIndicator().setValue("Y");
		}
		else
		{
			pid.getPatientDeathIndicator().setValue("N");
		}
	}

	private void buildKwaliteitsopnamePIDSegment(PID pid, MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto hl7BerichtTrigger) throws DataTypeException
	{

		pid.getSetIDPID().setValue("1");

		pid.getPid2_PatientID();

		pid.getPid3_PatientIdentifierList(0).getCx1_ID().setValue(hl7BerichtTrigger.getPatientID());
		pid.getPid3_PatientIdentifierList(0).getCx4_AssigningAuthority().getHd1_NamespaceID().setValue("LRCB");
		pid.getPid3_PatientIdentifierList(0).getCx5_IdentifierTypeCode().setValue("NNNLD");

		XPN patientGegevens = pid.getPid5_PatientName(0);
		patientGegevens.getGivenName().setValue("DUMMY");
		patientGegevens.getFamilyName().getSurname().setValue(hl7BerichtTrigger.getSeCode());
		patientGegevens.getFamilyName().getOwnSurnamePrefix().setValue("");
		patientGegevens.getFamilyName().getOwnSurname().setValue(hl7BerichtTrigger.getSeCode());

		pid.getPid7_DateTimeOfBirth().getTimeOfAnEvent().setValue(hl7DateFormatter.format(LocalDate.of(2018, 1, 1)));

		pid.getAdministrativeSex().setValue("U");

		pid.getPatientDeathIndicator().setValue("N");
	}

	private void buildPIDSegment(PID pid, Client client) throws DataTypeException
	{
		buildPIDSegment(pid, client, client.getPersoon().getBsn());
	}

	private void buildORCSegmentILM(ORC orc, MammaScreeningRonde ronde) throws DataTypeException
	{
		buildORCSegmentILM(orc, ronde.getUitnodigingsNr());
	}

	private void buildORCSegmentILM(ORC orc, Long accessionNumber) throws DataTypeException
	{
		orc.getOrc1_OrderControl().setValue("DC");
		orc.getOrc2_PlacerOrderNumber().getEi1_EntityIdentifier().setValue(String.valueOf(accessionNumber));
		orc.getOrc5_OrderStatus().setValue("IP");
	}

	private void buildORCSegment(ORC orc, String accessionNumber) throws DataTypeException
	{
		orc.getOrc1_OrderControl().setValue("XO");
		orc.getOrc2_PlacerOrderNumber().getEi1_EntityIdentifier().setValue(accessionNumber);
		orc.getOrc5_OrderStatus().setValue("SC");

	}

	private void buildKwaliteitsopnameORCSegment(ORC orc, MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto hl7BerichtTrigger) throws DataTypeException
	{
		orc.getOrc1_OrderControl().setValue("XO");
		orc.getOrc2_PlacerOrderNumber().getEi1_EntityIdentifier().setValue(hl7BerichtTrigger.getAccessionNumber());
		orc.getOrc5_OrderStatus().setValue("SC");

	}

	private void buildIlmOBRSegment(OBR obr, Client client, MammaScreeningRonde ronde, MammaHL7v24ORMBerichtStatus status) throws DataTypeException
	{
		buildOBRSegment(obr, ronde, status);
		obr.getObr2_PlacerOrderNumber().getEi1_EntityIdentifier().setValue(String.valueOf(ronde.getUitnodigingsNr()));
	}

	private void buildOBRSegment(OBR obr, MammaScreeningRonde ronde, MammaHL7v24ORMBerichtStatus status) throws DataTypeException
	{
		MammaAfspraak laatsteAfspraak = ronde.getLaatsteUitnodiging().getLaatsteAfspraak();
		MammaOnderzoek onderzoek = laatsteAfspraak.getOnderzoek();

		Date laatsteOnderzoekAfgerondOpDatum = onderzoek != null ? onderzoek.getAfgerondOp() : null;

		buildOBRSegment(
			obr,
			status,
			getAccessionNumber(ronde),
			"MAMMO",
			"Mammografie",
			laatsteOnderzoekAfgerondOpDatum,
			laatsteAfspraak.getVanaf(),
			laatsteAfspraak.getStandplaatsPeriode().getScreeningsEenheid().getCode());
	}

	private void buildUploadVerzoekOBRSegment(OBR obr, MammaHL7v24OrmBerichtTriggerUploadBeeldenDto hl7BerichtTrigger) throws DataTypeException
	{
		buildOBRSegment(
			obr,
			hl7BerichtTrigger.getStatus(),
			hl7BerichtTrigger.getAccessionNumber().toString(),
			"ZHOND",
			"Ziekenhuis",
			hl7BerichtTrigger.getOnderzoeksDatum(),
			null,
			"ZHOND");
	}

	private void buildOBRSegment(OBR obr, MammaHL7v24ORMBerichtStatus status, String accessionNumber, String usiIdentifier, String usiText, Date onderzoekAfgerondOp,
		Date laatsteAfspraakDatum, String condition) throws DataTypeException
	{
		obr.getObr2_PlacerOrderNumber().getEi1_EntityIdentifier().setValue(accessionNumber);
		obr.getObr4_UniversalServiceIdentifier().getCe1_Identifier().setValue(usiIdentifier);
		obr.getObr4_UniversalServiceIdentifier().getCe2_Text().setValue(usiText);
		obr.getObr5_Priority().setValue("R");
		if (onderzoekAfgerondOp != null && !MammaHL7v24ORMBerichtStatus.STARTED.equals(status))
		{
			obr.getObr7_ObservationDateTime().getTimeOfAnEvent().setValue(hl7DateTimeFormatter.format(DateUtil.toLocalDateTime(onderzoekAfgerondOp)));
		}

		obr.getObr25_ResultStatus().setValue(status.getLabel());
		if (laatsteAfspraakDatum != null)
		{
			obr.getObr27_QuantityTiming(0).getTq4_StartDateTime().getTimeOfAnEvent().setValue(hl7DateTimeFormatter.format(DateUtil.toLocalDateTime(laatsteAfspraakDatum)));
		}
		obr.getObr27_QuantityTiming(0).getTq7_Condition().setValue(condition);
	}

	private void buildKwaliteitsopnameOBRSegment(OBR obr, MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto hl7BerichtTrigger, MammaHL7v24ORMBerichtStatus status)
		throws DataTypeException
	{
		obr.getObr2_PlacerOrderNumber().getEi1_EntityIdentifier().setValue(hl7BerichtTrigger.getAccessionNumber());
		obr.getObr4_UniversalServiceIdentifier().getCe1_Identifier().setValue(hl7BerichtTrigger.getOnderzoekscode());
		obr.getObr4_UniversalServiceIdentifier().getCe2_Text().setValue("Kwaliteitsopname");
		obr.getObr5_Priority().setValue("R");

		obr.getObr25_ResultStatus().setValue(status.getLabel());
		obr.getObr27_QuantityTiming(0).getTq7_Condition().setValue(hl7BerichtTrigger.getSeCode());
		obr.getObr31_ReasonForStudy(0).getText().setValue(hl7BerichtTrigger.getReden());

	}

	private String getAccessionNumber(MammaScreeningRonde ronde)
	{
		return ronde.getUitnodigingsNr().toString();
	}
}
