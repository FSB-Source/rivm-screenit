package nl.rivm.screenit.wsb.pd.interceptor;

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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.URIResolver;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import lombok.extern.slf4j.Slf4j;

import net.sf.saxon.TransformerFactoryImpl;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.ws.providedocument.ProvideDocument;
import nl.rivm.screenit.ws.providedocument.ProvideDocument.DocumentMetaData;
import nl.rivm.screenit.ws.providedocument.ProvideDocument.DocumentMetaData.Project;
import nl.rivm.screenit.wsb.pd.PdConstants;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.cxf.binding.soap.SoapMessage;
import org.apache.cxf.binding.soap.interceptor.AbstractSoapInterceptor;
import org.apache.cxf.binding.xml.XMLFault;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.phase.Phase;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class SchematronInterceptor extends AbstractSoapInterceptor
{
	private final Map<String, Transformer> mdlTransformers = new HashMap<>();

	private final Map<String, Transformer> paTransformers = new HashMap<>();

	private final Map<String, Transformer> cervixCytologieTransformers = new HashMap<>();

	private final Map<String, Transformer> mammaFollowUpTransformers = new HashMap<>();

	List<Map<String, Transformer>> transformers = new ArrayList<>();

	private final List<String> supportedVersions = new ArrayList<>();

	private String currentSchematronVersionPathMapping = null;

	private final Map<String, String> schematronPathMapping = new HashMap<>();

	@Autowired
	@Qualifier("schematronLocation")
	private String schematronLocation;

	@Autowired
	private SimplePreferenceService preferenceService;

	public SchematronInterceptor()
	{
		super(Phase.PRE_INVOKE);
		transformers.add(mdlTransformers);
		transformers.add(paTransformers);
		transformers.add(cervixCytologieTransformers);
		transformers.add(mammaFollowUpTransformers);

	}

	@Override
	public void handleMessage(SoapMessage message) throws Fault
	{
		LOG.info("SchematronInterceptor");
		List<?> messageContentsList = message.getContent(List.class);
		DocumentMetaData metaData = null;
		String cda = null;
		Object ping = null;
		String projectVersion = "";
		if (messageContentsList != null)
		{
			for (Object messageContent : messageContentsList)
			{
				if (messageContent instanceof ProvideDocument)
				{
					ProvideDocument provideDocument = (ProvideDocument) messageContent;
					metaData = provideDocument.getDocumentMetaData();
					byte[] document = provideDocument.getDocument();
					if (document != null)
					{
						cda = new String(document);
					}
					ping = provideDocument.getPing();
				}
			}
		}
		if (ping != null)
		{
			LOG.info("ping ontvangen.");
			return;
		}
		if (metaData == null)
		{
			throw new XMLFault(Constants.XML_FAULT_PREFIX + "Schematron validation failed: kan geen meta data in SOAP vinden.");
		}
		if (cda == null)
		{
			throw new XMLFault(Constants.XML_FAULT_PREFIX + "Schematron validation failed: kan geen cda document in SOAP vinden.");
		}

		Project project = metaData.getProject();
		if (project != null)
		{
			projectVersion = project.getId() + "." + project.getVersion();
		}
		if (StringUtils.isBlank(projectVersion))
		{
			throw new XMLFault(Constants.XML_FAULT_PREFIX + "No projectversion found in meta data in SOAP.");
		}
		try
		{
			init(projectVersion);
		}
		catch (TransformerException | IOException e)
		{
			LOG.error("init failed", e);
			throw new XMLFault("init failed" + e.getMessage());
		}

		StringWriter result = new StringWriter();
		List<String> clinicalDocumentTemplateIds = metaData.getClinicalDocumentTemplateIds();
		String documentTemplateIdSOAP = null;
		if (CollectionUtils.isNotEmpty(clinicalDocumentTemplateIds))
		{
			documentTemplateIdSOAP = StringUtils.trim(clinicalDocumentTemplateIds.get(0));
		}
		validateCandidate(new StreamSource(new StringReader(cda)), new StreamResult(result), metaData.getClinicalDocumentCode().getCode(), documentTemplateIdSOAP, projectVersion);

		String stringResult = result.toString().trim().replaceAll("\n\n", "").replaceAll("\n", " ");

		if (StringUtils.isNotBlank(stringResult))
		{
			throw new XMLFault(Constants.XML_FAULT_PREFIX + "Schematron validation failed:" + stringResult);
		}
	}

	private void init(String projectVersion) throws TransformerException, IOException
	{
		String newSchematronPathMapping = preferenceService.getString(PreferenceKey.INTERNAL_WSB_SCHEMATRON_VERSIONPATHMAPPING.name());
		if (StringUtils.isNotBlank(newSchematronPathMapping))
		{
			if (!newSchematronPathMapping.equals(currentSchematronVersionPathMapping))
			{
				currentSchematronVersionPathMapping = newSchematronPathMapping.trim();
				schematronPathMapping.clear();
				supportedVersions.clear();
				transformers.forEach(t -> t.clear());
			}
		}
		if (schematronPathMapping.isEmpty())
		{
			String[] schematronPathMappings = currentSchematronVersionPathMapping.split(";");
			for (String schematronPathMappingParts : schematronPathMappings)
			{
				String[] schematronPathMappingPart = schematronPathMappingParts.split("\\|");
				if (schematronPathMappingPart.length == 2)
				{
					schematronPathMapping.put(schematronPathMappingPart[0], schematronPathMappingPart[1]);
				}
			}
		}
		String mappedVersionPath = schematronPathMapping.get(projectVersion);
		if (StringUtils.isBlank(mappedVersionPath))
		{
			LOG.error("Schematron " + projectVersion + " not found in mapping ('schematron/versionpathmapping').");
			throw new XMLFault("Schematron version " + projectVersion + " (project.id + '.' + project.version) unknown.");
		}
		if (!supportedVersions.contains(mappedVersionPath))
		{
			if (supportedVersions.size() == 4)
			{

				String oldestVersion = supportedVersions.remove(0);
				transformers.forEach(t -> t.remove(oldestVersion));
			}
			supportedVersions.add(mappedVersionPath);

		}

	}

	private byte[] compileSchematron(final String projectVersionPath, String type) throws TransformerConfigurationException, TransformerException, IOException
	{
		final Set<Closeable> closeables = new HashSet<>();
		try
		{

			TransformerFactory tf = new TransformerFactoryImpl();
			tf.setURIResolver(new URIResolver()
			{
				@Override
				public Source resolve(String href, String base) throws TransformerException
				{
					if (base.contains("include") || href.contains("include"))
					{
						String completeIncludePath = schematronLocation + "/" + projectVersionPath + "/" + href;
						try
						{
							FileInputStream fileInputStream = new FileInputStream(completeIncludePath);
							closeables.add(fileInputStream);
							return new StreamSource(fileInputStream);
						}
						catch (FileNotFoundException e)
						{
							LOG.error("Fout bij openen van bestand: " + completeIncludePath, e);
						}
					}
					else if (href.equals("iso_schematron_skeleton_for_saxon.xsl"))
					{
						InputStream inputStream = SchematronInterceptor.class.getResourceAsStream("/schematron-transform/iso_schematron_skeleton_for_saxon.xsl");
						closeables.add(inputStream);
						return new StreamSource(inputStream);
					}
					InputStream inputStream = SchematronInterceptor.class.getResourceAsStream(base + href);
					closeables.add(inputStream);
					return new StreamSource(inputStream);
				}
			});

			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			closeables.add(baos);
			byte[] interim = null;

			LOG.info("Running pre-process transform #1");
			InputStream inputStream = SchematronInterceptor.class.getResourceAsStream("/schematron-transform/iso_dsdl_include.xsl");
			closeables.add(inputStream);
			Source xsltSource = new StreamSource(inputStream);
			Transformer tempTransformer = tf.newTransformer(xsltSource);
			FileInputStream fileInputStream = new FileInputStream(schematronLocation + "/" + projectVersionPath + "/" + type);
			closeables.add(fileInputStream);
			tempTransformer.transform(new StreamSource(fileInputStream), new StreamResult(baos));
			interim = baos.toByteArray();
			baos.reset();

			LOG.info("Running pre-process transform #2");
			inputStream = SchematronInterceptor.class.getResourceAsStream("/schematron-transform/iso_abstract_expand.xsl");
			closeables.add(inputStream);
			xsltSource = new StreamSource(inputStream);
			tempTransformer = tf.newTransformer(xsltSource);
			tempTransformer.transform(new StreamSource(new ByteArrayInputStream(interim)), new StreamResult(baos));
			interim = baos.toByteArray();
			baos.reset();

			LOG.info("Transforming schema to XSLT");
			inputStream = SchematronInterceptor.class.getResourceAsStream("/schematron-transform/iso_svrl_for_xslt2.xsl");
			closeables.add(inputStream);
			xsltSource = new StreamSource(inputStream);
			tempTransformer = tf.newTransformer(xsltSource);
			tempTransformer.setParameter("full-path-notation", "4");
			tempTransformer.transform(new StreamSource(new ByteArrayInputStream(interim)), new StreamResult(baos));
			interim = baos.toByteArray();

			return interim;
		}
		finally
		{
			for (Closeable closeable : closeables)
			{
				try
				{
					closeable.close();
				}
				catch (Exception e)
				{
				}
			}
		}
	}

	private void validateCandidate(Source is, Result result, String docType, String templateId, String projectVersion)
	{
		try
		{
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			byte[] interim = null;
			StreamResult streamResult = new StreamResult(baos);
			TransformerFactory tf = new TransformerFactoryImpl();
			InputStream resourceAsStream = SchematronInterceptor.class.getResourceAsStream("/schematron-transform/svrl_transform.xsl");
			Transformer svrlTransformer = tf.newTransformer(new StreamSource(resourceAsStream));
			try
			{
				resourceAsStream.close();
			}
			catch (Exception e)
			{
			}

			LOG.debug("Applying XSLT to candidate");
			Transformer transformer = getTransformer(docType, templateId, projectVersion);
			transformer.transform(is, streamResult);

			interim = baos.toByteArray();
			try
			{
				baos.close();
			}
			catch (Exception e)
			{
			}

			ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(interim);
			svrlTransformer.transform(new StreamSource(byteArrayInputStream), result);
			try
			{
				byteArrayInputStream.close();
			}
			catch (Exception e)
			{
			}
		}
		catch (Exception e)
		{
			throw new IllegalStateException("Transformation failure: " + e.getMessage(), e);
		}
	}

	private Transformer getTransformer(String docType, String templateId, final String projectVersion) throws TransformerException, IOException
	{
		final Set<Closeable> closeables = new HashSet<>();

		final String projectVersionContext = schematronPathMapping.get(projectVersion);
		TransformerFactory tf = new TransformerFactoryImpl();
		tf.setURIResolver(new URIResolver()
		{

			@Override
			public Source resolve(String href, String base) throws TransformerException
			{
				if (base.contains("include") || href.contains("include"))
				{
					String completeIncludePath = schematronLocation + "/" + getSchematronPath(projectVersionContext) + "/" + href;
					try
					{
						FileInputStream fileInputStream = new FileInputStream(completeIncludePath);
						closeables.add(fileInputStream);
						return new StreamSource(fileInputStream);
					}
					catch (FileNotFoundException e)
					{
						LOG.error("Fout bij openen van bestand: " + completeIncludePath, e);
					}
				}
				else if (href.equals("iso_schematron_skeleton_for_saxon.xsl"))
				{
					InputStream resourceAsStream = SchematronInterceptor.class.getResourceAsStream("/schematron-transform/iso_schematron_skeleton_for_saxon.xsl");
					closeables.add(resourceAsStream);
					return new StreamSource(resourceAsStream);
				}
				InputStream resourceAsStream = SchematronInterceptor.class.getResourceAsStream(base + href);
				closeables.add(resourceAsStream);
				return new StreamSource(resourceAsStream);
			}

		});
		Transformer transformer = null;
		if (PdConstants.DOC_TYPE_MDL.equals(docType))
		{
			transformer = mdlTransformers.get(projectVersionContext);
			if (transformer == null)
			{
				LOG.info("Start compiling MDL schematron voor versie " + projectVersion);
				byte[] interim = compileSchematron(getSchematronPath(projectVersionContext), getSchematron(projectVersionContext, BerichtType.MDL_VERSLAG));
				LOG.info("Compiling MDL schematron finished voor versie " + projectVersion);
				ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(interim);
				closeables.add(byteArrayInputStream);
				Source xsltSource = new StreamSource(byteArrayInputStream);
				transformer = tf.newTransformer(xsltSource);
				mdlTransformers.put(projectVersionContext, transformer);
			}
		}
		else if (PdConstants.DOC_TYPE_PA.equals(docType) && (templateId == null || templateId.equals(PdConstants.TEMPLATE_ID_PA_DK)))
		{
			transformer = paTransformers.get(projectVersionContext);
			if (transformer == null)
			{
				LOG.info("Start compiling PA schematron voor versie " + projectVersion);
				byte[] interim = compileSchematron(getSchematronPath(projectVersionContext), getSchematron(projectVersionContext, BerichtType.PA_LAB_VERSLAG));
				LOG.info("Compiling PA schematron finished voor versie " + projectVersion);
				ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(interim);
				closeables.add(byteArrayInputStream);
				Source xsltSource = new StreamSource(byteArrayInputStream);
				transformer = tf.newTransformer(xsltSource);
				paTransformers.put(projectVersionContext, transformer);
			}
		}
		else if (PdConstants.DOC_TYPE_PA.equals(docType) && PdConstants.TEMPLATE_ID_PA_CYTOLOGY.equals(templateId))
		{
			transformer = cervixCytologieTransformers.get(projectVersionContext);
			if (transformer == null)
			{
				LOG.info("Start compiling Cervix Cytologie schematron voor versie " + projectVersion);
				byte[] interim = compileSchematron(getSchematronPath(projectVersionContext), getSchematron(projectVersionContext, BerichtType.CERVIX_CYTOLOGIE_VERSLAG));
				LOG.info("Compiling Cervix Cytologie schematron finished voor versie " + projectVersion);
				ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(interim);
				closeables.add(byteArrayInputStream);
				Source xsltSource = new StreamSource(byteArrayInputStream);
				transformer = tf.newTransformer(xsltSource);
				cervixCytologieTransformers.put(projectVersionContext, transformer);
			}
		}
		else if (PdConstants.DOC_TYPE_PA_BK_FOLLOW_UP.equals(docType) && PdConstants.TEMPLATE_ID_PA_FOLLOW_UP.equals(templateId))
		{
			transformer = mammaFollowUpTransformers.get(projectVersionContext);
			if (transformer == null)
			{
				LOG.info("Start compiling Mamma Follow UP schematron voor versie " + projectVersion);
				byte[] interim = compileSchematron(getSchematronPath(projectVersionContext), getSchematron(projectVersionContext, BerichtType.MAMMA_PA_FOLLOW_UP_VERSLAG));
				LOG.info("Compiling Mamma Follow UP schematron finished voor versie " + projectVersion);
				ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(interim);
				closeables.add(byteArrayInputStream);
				Source xsltSource = new StreamSource(byteArrayInputStream);
				transformer = tf.newTransformer(xsltSource);
				mammaFollowUpTransformers.put(projectVersionContext, transformer);
			}
		}
		else
		{
			throw new XMLFault("Schematron validation failed: document type " + docType + " in codeSystem " + PdConstants.OID_DOC_TYPE + " onbekend");
		}

		for (Closeable closeable : closeables)
		{
			try
			{
				closeable.close();
			}
			catch (Exception e)
			{
			}
		}

		return transformer;
	}

	private String getSchematronPath(String projectVersionContext)
	{
		String[] splittedContext = projectVersionContext.split(",");
		return splittedContext[0];
	}

	private String getSchematron(String projectVersionContext, BerichtType berichtType)
	{
		String keyword = null;
		switch (berichtType)
		{
		case MDL_VERSLAG:
			keyword = "-mdl";
			break;
		case PA_LAB_VERSLAG:
			keyword = "-pa";
			break;
		case CERVIX_CYTOLOGIE_VERSLAG:
			keyword = "-bmhk";
			break;
		case MAMMA_PA_FOLLOW_UP_VERSLAG:
			keyword = "-fuppa";
			break;
		}

		String[] splittedContext = projectVersionContext.split(",");
		for (int i = 1; i < splittedContext.length; i++)
		{
			String schematronEntry = splittedContext[i];
			if (schematronEntry.contains(keyword))
			{
				return schematronEntry;
			}
		}
		return null;
	}
}
