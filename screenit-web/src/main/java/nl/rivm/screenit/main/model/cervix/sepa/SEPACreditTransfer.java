package nl.rivm.screenit.main.model.cervix.sepa;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.OutputStream;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.UUID;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeConstants;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import nl.rivm.screenit.generated.sepa.AccountIdentification4Choice;
import nl.rivm.screenit.generated.sepa.ActiveOrHistoricCurrencyAndAmount;
import nl.rivm.screenit.generated.sepa.AmountType3Choice;
import nl.rivm.screenit.generated.sepa.BranchAndFinancialInstitutionIdentification4;
import nl.rivm.screenit.generated.sepa.CashAccount16;
import nl.rivm.screenit.generated.sepa.ChargeBearerType1Code;
import nl.rivm.screenit.generated.sepa.CreditTransferTransactionInformation10;
import nl.rivm.screenit.generated.sepa.CustomerCreditTransferInitiationV03;
import nl.rivm.screenit.generated.sepa.Document;
import nl.rivm.screenit.generated.sepa.FinancialInstitutionIdentification7;
import nl.rivm.screenit.generated.sepa.GenericFinancialIdentification1;
import nl.rivm.screenit.generated.sepa.GroupHeader32;
import nl.rivm.screenit.generated.sepa.ObjectFactory;
import nl.rivm.screenit.generated.sepa.PartyIdentification32;
import nl.rivm.screenit.generated.sepa.PaymentIdentification1;
import nl.rivm.screenit.generated.sepa.PaymentInstructionInformation3;
import nl.rivm.screenit.generated.sepa.PaymentMethod3Code;
import nl.rivm.screenit.generated.sepa.PaymentTypeInformation19;
import nl.rivm.screenit.generated.sepa.RemittanceInformation5;
import nl.rivm.screenit.generated.sepa.ServiceLevel8Choice;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang3.StringUtils;

import static com.google.common.base.Preconditions.checkArgument;

public class SEPACreditTransfer
{

	private static final Pattern bicRegex = Pattern.compile("([a-zA-Z]{4}[a-zA-Z]{2}[a-zA-Z0-9]{2}([a-zA-Z0-9]{3})?)");

	private final Document document = new Document();

	private final CustomerCreditTransferInitiationV03 customerCreditTransferInitiation;

	private GroupHeader32 groupHeader;

	public SEPACreditTransfer()
	{
		customerCreditTransferInitiation = new CustomerCreditTransferInitiationV03();
		document.setCstmrCdtTrfInitn(customerCreditTransferInitiation);
	}

	public void write(OutputStream os) throws JAXBException
	{
		JAXBContext jc = JAXBContext.newInstance(Document.class);
		Marshaller marshaller = jc.createMarshaller();
		marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);

		marshaller.setProperty(Marshaller.JAXB_ENCODING, "UTF-8");
		marshaller.marshal(new ObjectFactory().createDocument(document), os);
	}

	public void buildGroupHeader(String msgId, String name, Date date)
	{
		groupHeader = new GroupHeader32();

		if (msgId == null)
			msgId = UUID.randomUUID().toString().replaceAll("-", "");
		checkArgument(msgId.length() <= 35, "length of msgId is more than 35");
		checkArgument(msgId.length() > 1, "length of msgId is less than 1");
		groupHeader.setMsgId(msgId);

		groupHeader.setCreDtTm(createXMLGregorianCalendar(date));

		groupHeader.setNbOfTxs("0");

		groupHeader.setCtrlSum(BigDecimal.ZERO);

		groupHeader.setInitgPty(createParty(name));

		customerCreditTransferInitiation.setGrpHdr(groupHeader);
	}

	public Betaalgroep betaalgroep(
		String pmtInfId, LocalDate reqdExctnDt,
		String debtorNm, String debtorAccountIBAN, String financialInstitutionBIC)
	{

		checkArgument(pmtInfId.length() <= 35, "length of pmtInfId is more than 35");
		checkArgument(pmtInfId.length() > 1, "length of pmtInfId is less than 1");

		PaymentInstructionInformation3 paymentInstructionInformation = new PaymentInstructionInformation3();

		paymentInstructionInformation.setPmtInfId(pmtInfId);

		paymentInstructionInformation.setPmtMtd(PaymentMethod3Code.TRF);

		paymentInstructionInformation.setNbOfTxs("0");

		paymentInstructionInformation.setCtrlSum(BigDecimal.ZERO);

		PaymentTypeInformation19 paymentTypeInformation = new PaymentTypeInformation19();
		ServiceLevel8Choice serviceLevel8Choice = new ServiceLevel8Choice();
		serviceLevel8Choice.setCd("SEPA");
		paymentTypeInformation.setSvcLvl(serviceLevel8Choice);
		paymentInstructionInformation.setPmtTpInf(paymentTypeInformation);

		paymentInstructionInformation.setReqdExctnDt(createXMLGregorianCalendarDate(DateUtil.toUtilDate(reqdExctnDt)));

		paymentInstructionInformation.setDbtr(createParty(debtorNm));

		paymentInstructionInformation.setDbtrAcct(createAccount(debtorAccountIBAN));

		paymentInstructionInformation.setDbtrAgt(createFinInstnId(financialInstitutionBIC));

		paymentInstructionInformation.setChrgBr(ChargeBearerType1Code.SLEV);

		customerCreditTransferInitiation.getPmtInf().add(paymentInstructionInformation);

		return new Betaalgroep(paymentInstructionInformation);
	}

	public class Betaalgroep
	{

		private PaymentInstructionInformation3 paymentInstructionInformation3;

		public Betaalgroep(PaymentInstructionInformation3 paymentInstructionInformation3)
		{
			this.paymentInstructionInformation3 = paymentInstructionInformation3;
		}

		public Betaalgroep creditTransfer(String endToEndId, BigDecimal amount,
			String creditorfinancialInstitutionBic,
			String creditorNm, String iban,
			String text)
		{

			CreditTransferTransactionInformation10 creditTransferTransactionInformation = new CreditTransferTransactionInformation10();

			PaymentIdentification1 paymentIdentification = new PaymentIdentification1();
			paymentIdentification.setEndToEndId(endToEndId);
			creditTransferTransactionInformation.setPmtId(paymentIdentification);

			creditTransferTransactionInformation.setAmt(createAmount(amount));

			creditTransferTransactionInformation.setCdtrAgt(createFinInstnId(creditorfinancialInstitutionBic));

			creditTransferTransactionInformation.setCdtr(createParty(creditorNm));

			creditTransferTransactionInformation.setCdtrAcct(createAccount(iban));

			creditTransferTransactionInformation.setRmtInf(createRmtInf(text));

			paymentInstructionInformation3.getCdtTrfTxInf().add(creditTransferTransactionInformation);

			paymentInstructionInformation3.setCtrlSum(paymentInstructionInformation3.getCtrlSum().add(amount));
			groupHeader.setCtrlSum(groupHeader.getCtrlSum().add(amount));

			paymentInstructionInformation3.setNbOfTxs(String.valueOf(paymentInstructionInformation3.getCdtTrfTxInf().size()));
			Integer nbOfTxs = Integer.parseInt(groupHeader.getNbOfTxs());
			nbOfTxs = nbOfTxs + 1;
			groupHeader.setNbOfTxs(nbOfTxs.toString());

			return this;
		}

	}

	public static PartyIdentification32 createParty(String nm)
	{
		PartyIdentification32 party = new PartyIdentification32();
		party.setNm(nm);
		return party;
	}

	public static XMLGregorianCalendar createXMLGregorianCalendar(Date currentDateTime)
	{
		GregorianCalendar calendar = new GregorianCalendar();
		calendar.setTime(currentDateTime);

		XMLGregorianCalendar createDate;
		try
		{
			createDate = DatatypeFactory.newInstance().newXMLGregorianCalendar(calendar);
			createDate.setMillisecond(DatatypeConstants.FIELD_UNDEFINED);
			createDate.setTimezone(DatatypeConstants.FIELD_UNDEFINED);
		}
		catch (DatatypeConfigurationException e)
		{
			throw new RuntimeException(e);
		}

		return createDate;
	}

	public static XMLGregorianCalendar createXMLGregorianCalendarDate(Date currentDateTime)
	{
		GregorianCalendar calendar = new GregorianCalendar();
		calendar.setTime(currentDateTime);

		XMLGregorianCalendar createDate;
		try
		{
			createDate = DatatypeFactory.newInstance().newXMLGregorianCalendarDate(
				calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH) + 1, calendar.get(Calendar.DAY_OF_MONTH),
				DatatypeConstants.FIELD_UNDEFINED);
		}
		catch (DatatypeConfigurationException e)
		{
			throw new RuntimeException(e);
		}

		return createDate;
	}

	public static CashAccount16 createAccount(String iban)
	{

		CashAccount16 account = new CashAccount16();
		AccountIdentification4Choice creditorAccountId = new AccountIdentification4Choice();

		creditorAccountId.setIBAN(iban);
		account.setId(creditorAccountId);
		return account;
	}

	public static BranchAndFinancialInstitutionIdentification4 createFinInstnId(String bic)
	{
		BranchAndFinancialInstitutionIdentification4 creditorAgent = new BranchAndFinancialInstitutionIdentification4();
		FinancialInstitutionIdentification7 creditorfinancialInstitutionIdentification = new FinancialInstitutionIdentification7();

		if (bic == null)
		{
			GenericFinancialIdentification1 othrId = new GenericFinancialIdentification1();
			othrId.setId("NOTPROVIDED");
			creditorfinancialInstitutionIdentification.setOthr(othrId);
		}
		else if (!StringUtils.isEmpty(bic))
		{
			checkArgument(bicRegex.matcher(bic).matches(), "This doesn't look like a correct BIC id '" + bic + "'");
			creditorfinancialInstitutionIdentification.setBIC(bic);
		}
		creditorAgent.setFinInstnId(creditorfinancialInstitutionIdentification);
		return creditorAgent;
	}

	public static AmountType3Choice createAmount(BigDecimal amount)
	{
		AmountType3Choice amt = new AmountType3Choice();
		ActiveOrHistoricCurrencyAndAmount instdAmt = new ActiveOrHistoricCurrencyAndAmount();
		instdAmt.setValue(amount);
		instdAmt.setCcy("EUR");
		amt.setInstdAmt(instdAmt);
		return amt;
	}

	public static RemittanceInformation5 createRmtInf(String info)
	{
		checkArgument(info.length() <= 140); 
		checkArgument(info.length() >= 1); 

		RemittanceInformation5 remittanceInformation = new RemittanceInformation5();
		remittanceInformation.getUstrd().add(info);
		return remittanceInformation;
	}

}
