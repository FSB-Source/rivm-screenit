package nl.rivm.screenit.wsb.pd.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.xml.ws.Holder;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.hl7v3.cda.ClinicalDocument;
import nl.rivm.screenit.hl7v3.cda.II;
import nl.rivm.screenit.hl7v3.cda.helper.CDAHelper;
import nl.rivm.screenit.hl7v3.cda.helper.ExtractCDA;
import nl.rivm.screenit.model.berichten.cda.CdaConstants;
import nl.rivm.screenit.model.berichten.cda.CdaOID;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.cda.PdBerichtResponseCode;
import nl.rivm.screenit.model.berichten.cda.PdBerichtResponseResult;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.logging.BerichtOntvangenLogEvent;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.ws.providedocument.ProvideDocument;
import nl.rivm.screenit.ws.providedocument.ProvideDocument.DocumentMetaData;
import nl.rivm.screenit.ws.providedocument.ProvideDocument.DocumentMetaData.Project;
import nl.rivm.screenit.ws.providedocument.ProvideDocumentPortType;
import nl.rivm.screenit.ws.providedocument.ProvideDocumentResponse;
import nl.rivm.screenit.wsb.pd.PdConstants;
import nl.rivm.screenit.wsb.service.CdaVerslagService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.cxf.annotations.SchemaValidation;
import org.apache.cxf.message.Message;
import org.apache.cxf.phase.PhaseInterceptorChain;
import org.apache.cxf.transport.http.AbstractHTTPDestination;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
@SchemaValidation
@Slf4j
@AllArgsConstructor
public class ProvideDocumentPortTypeImpl implements ProvideDocumentPortType
{

	private final HibernateService hibernateService;

	private final CdaVerslagService cdaVerslagService;

	private final LogService logService;

	private final ICurrentDateSupplier currentDateSupplier;

	private void provideDocument(DocumentMetaData documentMetaData, byte[] document, Holder<Boolean> success, Holder<String> code, Holder<String> text, String remoteAddr)
	{
		LOG.info("ProvideDocumentPortTypeImpl");
		success.value = false;
		String cda = new String(document);

		try
		{
			ClinicalDocument cdaDocument = ExtractCDA.getCDADocument(cda);

			if (checkConsitentie(documentMetaData, success, code, text, cdaDocument))
			{
				success.value = false;
				String documentTypeCodeSOAP = documentMetaData.getClinicalDocumentCode().getCode();
				List<String> clinicalDocumentTemplateIds = documentMetaData.getClinicalDocumentTemplateIds();
				String documentTemplateIdSOAP = null;
				if (CollectionUtils.isNotEmpty(clinicalDocumentTemplateIds))
				{
					documentTemplateIdSOAP = StringUtils.trim(clinicalDocumentTemplateIds.get(0));
				}
				String berichtIdSOAP = documentMetaData.getClinicalDocumentId().getRoot() + "." + documentMetaData.getClinicalDocumentId().getExtension();
				String setIdSOAP = documentMetaData.getClinicalDocumentSetId().getRoot() + "." + documentMetaData.getClinicalDocumentSetId().getExtension();
				Long versieSOAP = Long.valueOf(documentMetaData.getClinicalDocumentVersionNumber().longValue());
				String bsnSOAP = documentMetaData.getPatientId().getExtension();

				BerichtToBatchService verwerkBerichtService = SpringBeanProvider.getInstance().getBean(BerichtToBatchService.class);

				OntvangenCdaBericht ontvangenCdaBericht = new OntvangenCdaBericht();
				if (PdConstants.DOC_TYPE_MDL.equals(documentTypeCodeSOAP))
				{
					ontvangenCdaBericht.setBerichtType(BerichtType.MDL_VERSLAG);
				}
				else if (PdConstants.DOC_TYPE_PA.equals(documentTypeCodeSOAP) && (documentTemplateIdSOAP == null || documentTemplateIdSOAP.equals(PdConstants.TEMPLATE_ID_PA_DK)))
				{
					ontvangenCdaBericht.setBerichtType(BerichtType.PA_LAB_VERSLAG);
				}
				else if (PdConstants.DOC_TYPE_PA.equals(documentTypeCodeSOAP) && PdConstants.TEMPLATE_ID_PA_CYTOLOGY.equals(documentTemplateIdSOAP))
				{
					ontvangenCdaBericht.setBerichtType(BerichtType.CERVIX_CYTOLOGIE_VERSLAG);
				}
				else if (PdConstants.DOC_TYPE_PA_BK_FOLLOW_UP.equals(documentTypeCodeSOAP) && PdConstants.TEMPLATE_ID_PA_FOLLOW_UP.equals(documentTemplateIdSOAP))
				{
					ontvangenCdaBericht.setBerichtType(BerichtType.MAMMA_PA_FOLLOW_UP_VERSLAG);
				}
				ontvangenCdaBericht.setOntvangen(currentDateSupplier.getDate());
				ontvangenCdaBericht.setStatus(BerichtStatus.NIEUW);
				ontvangenCdaBericht.setXmlBericht(cda);
				ontvangenCdaBericht.setBerichtId(berichtIdSOAP);
				ontvangenCdaBericht.setSetId(setIdSOAP);
				ontvangenCdaBericht.setVersie(versieSOAP);

				Project project = documentMetaData.getProject();
				String projectVersie = project.getId() + "." + project.getVersion();
				ontvangenCdaBericht.setProjectVersion(projectVersie);

				String orgInfo = CDAHelper.getUitvoerendeOrganisatieInformatie(cdaDocument);
				PdBerichtResponseResult result = cdaVerslagService.valideerBericht(ontvangenCdaBericht, bsnSOAP, remoteAddr, orgInfo);

				code.value = result.getCode().name();
				text.value = result.getOmschrijving();
				if (PdBerichtResponseCode.OK.equals(result.getCode()))
				{
					BerichtType berichtType = ontvangenCdaBericht.getBerichtType();
					String melding = "Provided document met berichtId " + berichtIdSOAP + ", setId " + setIdSOAP + " en versie " + versieSOAP
						+ " is in goede orde ontvangen" + orgInfo + ". Wordt aangeboden aan batch process voor verdere verwerking.";
					BerichtOntvangenLogEvent logEvent = new BerichtOntvangenLogEvent();
					logEvent.setBericht(ontvangenCdaBericht);
					logEvent.setMelding(melding);

					Bevolkingsonderzoek bvo = berichtType.getBevolkingsonderzoek();
					logService.logGebeurtenis(berichtType.getLbBerichtOntvangen(), logEvent, bvo);

					try
					{
						ontvangenCdaBericht.setStatus(BerichtStatus.VERWERKING);
						hibernateService.saveOrUpdate(ontvangenCdaBericht);
						verwerkBerichtService.queueCDABericht(berichtType.getBevolkingsonderzoek());
						success.value = true;
					}
					catch (Exception e)
					{
						code.value = PdBerichtResponseCode.SYSTEM_ERROR.name();
						text.value = PdBerichtResponseCode.SYSTEM_ERROR + ": " + e.getMessage();
						LOG.error("Fout bij verwerking van bericht: ", e);
						ontvangenCdaBericht.setStatus(BerichtStatus.FOUT);
						hibernateService.saveOrUpdate(ontvangenCdaBericht);
					}
				}
				else if (PdBerichtResponseCode.REEDS_CORRECT_VERWERKT.equals(result.getCode()))
				{
					success.value = true;
				}
			}
		}
		catch (Exception e)
		{
			code.value = PdBerichtResponseCode.SYSTEM_ERROR.name();
			text.value = PdBerichtResponseCode.SYSTEM_ERROR + ": Onverwachte fout.";
			LOG.error("Fout bij verwerking van bericht: ", e);
		}
	}

	private boolean checkConsitentie(DocumentMetaData documentMetaData, Holder<Boolean> success, Holder<String> code, Holder<String> text, ClinicalDocument cdaDocument)
	{
		String documentTypeCodeSOAP = documentMetaData.getClinicalDocumentCode().getCode();
		String berichtIdSOAP = documentMetaData.getClinicalDocumentId().getRoot() + "." + documentMetaData.getClinicalDocumentId().getExtension();
		String setIdSOAP = documentMetaData.getClinicalDocumentSetId().getRoot() + "." + documentMetaData.getClinicalDocumentSetId().getExtension();
		Long versieSOAP = Long.valueOf(documentMetaData.getClinicalDocumentVersionNumber().longValue());
		String bsnSOAP = documentMetaData.getPatientId().getExtension();
		String custodianSOAP = documentMetaData.getCustodian().getRoot();
		if (StringUtils.isNotBlank(documentMetaData.getCustodian().getExtension()))
		{
			custodianSOAP += "." + documentMetaData.getCustodian().getExtension().trim();
		}

		String documentTypeCodeCda = cdaDocument.getCode().getCode();
		String berichtIdCda = cdaDocument.getId().getRoot() + "." + cdaDocument.getId().getExtension();
		String setIdCda = cdaDocument.getSetId().getRoot() + "." + cdaDocument.getSetId().getExtension();
		Long versieCda = cdaDocument.getVersionNumber().getValue().longValue();
		List<II> ids = CDAHelper.getAllValues(cdaDocument, CdaConstants.PATIENT_IDS_PATH);
		String bsnCda = CDAHelper.getExtension(CdaOID.BSN, ids);

		ids = CDAHelper.getAllValues(cdaDocument, CdaConstants.CUSTODIAN_URA_PATH);
		String custodianCda = "";
		if (ids != null && ids.size() == 1)
		{
			custodianCda = CDAHelper.getRootExtension(ids.get(0));
		}

		success.value = checkFieldConsitentie(documentTypeCodeSOAP, documentTypeCodeCda, "DocumentTypeCode", code, text);
		if (success.value && !checkFieldConsitentie(berichtIdSOAP, berichtIdCda, "BerichtId", code, text))
		{
			success.value = false;
		}
		else if (success.value && !checkFieldConsitentie(setIdSOAP, setIdCda, "SetId", code, text))
		{
			success.value = false;
		}
		else if (success.value && !checkFieldConsitentie(versieSOAP, versieCda, "Versie", code, text))
		{
			success.value = false;
		}
		else if (success.value && !checkFieldConsitentie(bsnSOAP, bsnCda, "BSN", code, text))
		{
			success.value = false;
		}
		else if (success.value && !checkFieldConsitentie(custodianSOAP, custodianCda, "Custodian", code, text))
		{
			success.value = false;
		}
		return success.value;
	}

	private Boolean checkFieldConsitentie(Object soap, Object cda, String fieldName, Holder<String> code, Holder<String> text)
	{
		if (soap != null && !soap.equals(cda))
		{
			String codeName = PdBerichtResponseCode.CDA_SOAP_INCONSISTENT.name();
			String codeOmschrijvingFormat = PdBerichtResponseCode.CDA_SOAP_INCONSISTENT.toString();
			code.value = codeName;
			text.value = String.format(codeOmschrijvingFormat, fieldName, soap, fieldName, cda);
			return false;
		}
		return true;
	}

	@Override
	public ProvideDocumentResponse provideDocument(ProvideDocument body)
	{
		String remoteAddr = "<Unkown>";
		try
		{
			Message message = PhaseInterceptorChain.getCurrentMessage();
			if (message != null)
			{
				HttpServletRequest request = (HttpServletRequest) message.get(AbstractHTTPDestination.HTTP_REQUEST);
				String[] headerAttrs = new String[] { "X-Forwarded-For", "Proxy-Client-IP", "WL-Proxy-Client-IP", "HTTP_CLIENT_IP", "HTTP_X_FORWARDED_FOR" };
				List<String> adressen = new ArrayList<>();
				for (String headerAttr : headerAttrs)
				{
					String headerAttrValue = request.getHeader(headerAttr);
					if (StringUtils.isNotBlank(headerAttrValue) && !"unknown".equalsIgnoreCase(headerAttrValue))
					{
						adressen.addAll(Arrays.asList(headerAttrValue.split(",")));
					}
				}

				if (adressen.isEmpty())
				{
					remoteAddr = request.getRemoteAddr();
				}
				else
				{
					remoteAddr = StringUtils.join(adressen, ", ");
				}
			}
		}
		catch (Exception e)
		{
			LOG.error("request niet succesvol gestuurd?", e.getMessage());
		}
		Holder<Boolean> success = new Holder<>(true);
		Holder<String> code = new Holder<>("PING_OK");
		Holder<String> text = new Holder<>("Ping succesvol");
		if (body.getPing() != null)
		{
			LOG.info("Ping ontvangen van " + remoteAddr);
		}
		else if (body.getDocument() != null && body.getDocumentMetaData() != null)
		{
			provideDocument(body.getDocumentMetaData(), body.getDocument(), success, code, text, remoteAddr);
		}
		else
		{
			success.value = false;
			code.value = "GEEN_PING_GEEN_CDA";
			text.value = "Geen ping maar ook geen CDA in SOAP";
			LOG.error("Geen ping maar ook geen CDA from " + remoteAddr);
		}
		ProvideDocumentResponse response = new ProvideDocumentResponse();
		response.setCode(code.value);
		response.setSuccess(success.value);
		response.setText(text.value);
		return response;
	}
}
