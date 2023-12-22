package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.NoSuchElementException;
import java.util.Optional;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.alg.client.contact.DigitaalBerichtDTO;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DigitaalBerichtTemplate;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.enums.DigitaalBerichtTemplateType;
import nl.rivm.screenit.model.enums.DigitaalBerichtType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.repository.algemeen.DigitaalBerichtTemplateRepository;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.DigitaalBerichtTemplateService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@AllArgsConstructor
@Service
@Transactional(propagation = Propagation.REQUIRED)
@Slf4j
public class DigitaalBerichtTemplateServiceImpl implements DigitaalBerichtTemplateService
{
	private ICurrentDateSupplier currentDateSupplier;

	private DigitaalBerichtTemplateRepository digitaalBerichtTemplateRepository;

	private HibernateService hibernateService;

	private ClientService clientService;

	private LogService logService;

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public Optional<DigitaalBerichtTemplate> findDigitaalBerichtTemplate(DigitaalBerichtTemplateType type)
	{
		return digitaalBerichtTemplateRepository.findDigitaalBerichtTemplateByType(type);
	}

	private DigitaalBerichtTemplate getDigitaalBerichtTemplate(DigitaalBerichtTemplateType type) throws NoSuchElementException
	{
		return findDigitaalBerichtTemplate(type).orElseThrow(() -> new NoSuchElementException("Templatetype: " + type + " staat niet in de database"));
	}

	@Override
	public void saveOrUpdateDigitaalBerichtTemplate(DigitaalBerichtTemplate template, Account account, String berichtTemplateTypeNaam)
	{
		template.setGewijzigdOp(currentDateSupplier.getLocalDateTime());
		digitaalBerichtTemplateRepository.save(template);
		maakLogGebeurtenisBijOpslaanTemplate(template, account, berichtTemplateTypeNaam);
	}

	@Override
	public DigitaalBerichtDTO maakDigitaalBericht(DigitaalBerichtTemplateType type, Client client)
	{
		var template = getDigitaalBerichtTemplate(type);
		var context = maakMailMergeContext(client);
		context.putValue(MailMergeContext.CONTEXT_MAMMA_CE, clientService.bepaalCe(client));

		return maakDigitaalBericht(template, context);
	}

	@Override
	public DigitaalBerichtDTO maakDigitaalBericht(DigitaalBerichtTemplateType type, ColoscopieCentrum intakelocatie)
	{
		var template = getDigitaalBerichtTemplate(type);
		var context = maakMailMergeContext(intakelocatie);

		return maakDigitaalBericht(template, context);
	}

	private void maakLogGebeurtenisBijOpslaanTemplate(DigitaalBerichtTemplate template, Account account, String berichtTemplateTypeNaam)
	{
		var verschilTekst = EntityAuditUtil.getDiffFieldsToLatestVersion(template, hibernateService.getHibernateSession());
		if (StringUtils.isNotBlank(verschilTekst))
		{
			var logBericht = String.format("Berichttemplate %s aangepast", berichtTemplateTypeNaam);
			logService.logGebeurtenis(LogGebeurtenis.PARAMETERISATIE_WIJZIG, account, logBericht);
		}
	}

	private MailMergeContext maakMailMergeContext(Client client)
	{
		var context = new MailMergeContext();
		context.setClient(client);
		return context;
	}

	private MailMergeContext maakMailMergeContext(ColoscopieCentrum intakelocatie)
	{
		var context = new MailMergeContext();
		context.setIntakelocatie(intakelocatie);
		return context;
	}

	private DigitaalBerichtDTO maakDigitaalBericht(DigitaalBerichtTemplate template, MailMergeContext mailMergeContext)
	{
		var templateType = template.getType();
		var mergefields = templateType.getMergeFields();
		var berichtDto = new DigitaalBerichtDTO();

		var bodyTekst = template.getBody();
		var subjectTekst = template.getSubject();

		for (var mergeField : mergefields)
		{
			String searchString = "{" + mergeField.getFieldName() + "}";
			if (bodyTekst.contains(searchString) || subjectTekst != null && subjectTekst.contains(searchString))
			{
				var mergeFieldString = zetMergeFieldValueString(mailMergeContext, mergeField);

				bodyTekst = StringUtils.replace(bodyTekst, searchString, mergeFieldString);
				if (DigitaalBerichtType.SMS != templateType.getBerichtType() && subjectTekst != null)
				{
					subjectTekst = StringUtils.replace(subjectTekst, searchString, mergeFieldString);
				}
			}
		}
		if (templateType.getMergeMailAttachmentService() != null)
		{
			var mergeMailService = SpringBeanProvider.getInstance().getBean(templateType.getMergeMailAttachmentService());
			var attachmentDtos = mergeMailService.maakMailAttachmentsAan(mailMergeContext, template);
			berichtDto.setAttachments(attachmentDtos);
		}
		berichtDto.setBody(bodyTekst);
		berichtDto.setSubject(subjectTekst);

		return berichtDto;
	}

	private String zetMergeFieldValueString(MailMergeContext mailMergeContext, MergeField mergeField)
	{
		Object mergeFieldValue = mergeField.getValue(mailMergeContext);

		if (mergeFieldValue != null)
		{
			return mergeFieldValue.toString();
		}
		return "";
	}
}
