package nl.rivm.screenit.wsb.pd.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.model.logging.BerichtOntvangenLogEvent;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.ws.providedocument.ProvideDocument;
import nl.rivm.screenit.ws.providedocument.ProvideDocument.DocumentMetaData;
import nl.rivm.screenit.ws.providedocument.ProvideDocumentPortType;
import nl.rivm.screenit.ws.providedocument.ProvideDocumentResponse;
import nl.rivm.screenit.wsb.pd.PdConstants;
import nl.rivm.screenit.wsb.service.CdaVerslagService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.cxf.annotations.SchemaValidation;
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
		success.value = false;
		var cda = new String(document);

		try
		{
			var cdaDocument = ExtractCDA.getCDADocument(cda);

			if (checkConsistentie(documentMetaData, success, code, text, cdaDocument))
			{
				success.value = false;
				var documentTypeCodeSOAP = documentMetaData.getClinicalDocumentCode().getCode();
				var clinicalDocumentTemplateIds = documentMetaData.getClinicalDocumentTemplateIds();
				String documentTemplateIdSOAP = null;
				if (CollectionUtils.isNotEmpty(clinicalDocumentTemplateIds))
				{
					documentTemplateIdSOAP = StringUtils.trim(clinicalDocumentTemplateIds.get(0));
				}
				var berichtIdSOAP = documentMetaData.getClinicalDocumentId().getRoot() + "." + documentMetaData.getClinicalDocumentId().getExtension();
				var setIdSOAP = documentMetaData.getClinicalDocumentSetId().getRoot() + "." + documentMetaData.getClinicalDocumentSetId().getExtension();
				var versieSOAP = Long.valueOf(documentMetaData.getClinicalDocumentVersionNumber().longValue());
				var bsnSOAP = documentMetaData.getPatientId().getExtension();

				var verwerkBerichtService = ApplicationContextProvider.getApplicationContext().getBean(BerichtToBatchService.class);

				var ontvangenCdaBericht = new OntvangenCdaBericht();
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

				var project = documentMetaData.getProject();
				var projectVersie = project.getId() + "." + project.getVersion();
				ontvangenCdaBericht.setProjectVersion(projectVersie);

				var orgInfo = CDAHelper.getUitvoerendeOrganisatieInformatie(cdaDocument);
				var result = cdaVerslagService.valideerBericht(ontvangenCdaBericht, bsnSOAP, remoteAddr, orgInfo);

				code.value = result.getCode().name();
				text.value = result.getOmschrijving();
				if (PdBerichtResponseCode.OK.equals(result.getCode()))
				{
					var berichtType = ontvangenCdaBericht.getBerichtType();
					var melding = "Provided document met berichtId " + berichtIdSOAP + ", setId " + setIdSOAP + " en versie " + versieSOAP
						+ " is in goede orde ontvangen" + orgInfo + ". Wordt aangeboden aan batch process voor verdere verwerking.";
					var logEvent = new BerichtOntvangenLogEvent();
					logEvent.setBericht(ontvangenCdaBericht);
					logEvent.setMelding(melding);

					var bvo = berichtType.getBevolkingsonderzoek();
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

	private boolean checkConsistentie(DocumentMetaData documentMetaData, Holder<Boolean> success, Holder<String> code, Holder<String> text, ClinicalDocument cdaDocument)
	{
		var documentTypeCodeSOAP = documentMetaData.getClinicalDocumentCode().getCode();
		var berichtIdSOAP = documentMetaData.getClinicalDocumentId().getRoot() + "." + documentMetaData.getClinicalDocumentId().getExtension();
		var setIdSOAP = documentMetaData.getClinicalDocumentSetId().getRoot() + "." + documentMetaData.getClinicalDocumentSetId().getExtension();
		var versieSOAP = Long.valueOf(documentMetaData.getClinicalDocumentVersionNumber().longValue());
		var bsnSOAP = documentMetaData.getPatientId().getExtension();
		var custodianSOAP = documentMetaData.getCustodian().getRoot();
		if (StringUtils.isNotBlank(documentMetaData.getCustodian().getExtension()))
		{
			custodianSOAP += "." + documentMetaData.getCustodian().getExtension().trim();
		}

		var documentTypeCodeCda = cdaDocument.getCode().getCode();
		var berichtIdCda = cdaDocument.getId().getRoot() + "." + cdaDocument.getId().getExtension();
		var setIdCda = cdaDocument.getSetId().getRoot() + "." + cdaDocument.getSetId().getExtension();
		var versieCda = cdaDocument.getVersionNumber().getValue().longValue();
		List<II> ids = CDAHelper.getAllValues(cdaDocument, CdaConstants.PATIENT_IDS_PATH);
		var bsnCda = CDAHelper.getExtension(CdaOID.BSN, ids);

		ids = CDAHelper.getAllValues(cdaDocument, CdaConstants.CUSTODIAN_URA_PATH);
		var custodianCda = "";
		if (ids.size() == 1)
		{
			custodianCda = CDAHelper.getRootExtension(ids.get(0));
		}

		success.value = checkFieldConsistentie(documentTypeCodeSOAP, documentTypeCodeCda, "DocumentTypeCode", code, text);
		if (success.value && !checkFieldConsistentie(berichtIdSOAP, berichtIdCda, "BerichtId", code, text))
		{
			success.value = false;
		}
		else if (success.value && !checkFieldConsistentie(setIdSOAP, setIdCda, "SetId", code, text))
		{
			success.value = false;
		}
		else if (success.value && !checkFieldConsistentie(versieSOAP, versieCda, "Versie", code, text))
		{
			success.value = false;
		}
		else if (success.value && !checkFieldConsistentie(bsnSOAP, bsnCda, "BSN", code, text))
		{
			success.value = false;
		}
		else if (success.value && !checkFieldConsistentie(custodianSOAP, custodianCda, "Custodian", code, text))
		{
			success.value = false;
		}
		return success.value;
	}

	private Boolean checkFieldConsistentie(Object soap, Object cda, String fieldName, Holder<String> code, Holder<String> text)
	{
		if (soap != null && !soap.equals(cda))
		{
			var codeName = PdBerichtResponseCode.CDA_SOAP_INCONSISTENT.name();
			var codeOmschrijvingFormat = PdBerichtResponseCode.CDA_SOAP_INCONSISTENT.toString();
			code.value = codeName;
			text.value = String.format(codeOmschrijvingFormat, fieldName, soap, fieldName, cda);
			return false;
		}
		return true;
	}

	@Override
	public ProvideDocumentResponse provideDocument(ProvideDocument body)
	{
		var remoteAddr = "<Unkown>";
		try
		{
			var message = PhaseInterceptorChain.getCurrentMessage();
			if (message != null)
			{
				var request = (HttpServletRequest) message.get(AbstractHTTPDestination.HTTP_REQUEST);
				var headerAttrs = new String[] { "X-Forwarded-For", "Proxy-Client-IP", "WL-Proxy-Client-IP", "HTTP_CLIENT_IP", "HTTP_X_FORWARDED_FOR" };
				var adressen = new ArrayList<String>();
				for (var headerAttr : headerAttrs)
				{
					var headerAttrValue = request.getHeader(headerAttr);
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
			LOG.error("request niet succesvol gestuurd?", e);
		}
		var success = new Holder<>(true);
		var code = new Holder<>("PING_OK");
		var text = new Holder<>("Ping succesvol");
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
			LOG.error("Geen ping maar ook geen CDA van {}", remoteAddr);
		}
		var response = new ProvideDocumentResponse();
		response.setCode(code.value);
		response.setSuccess(success.value);
		response.setText(text.value);
		return response;
	}
}
