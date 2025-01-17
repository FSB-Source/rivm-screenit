package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Optional;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.alg.client.contact.MailAttachmentDto;
import nl.rivm.screenit.model.DigitaalBerichtTemplate;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.enums.DigitaalBerichtTemplateType;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.service.AfspraakIcalService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaMergeMailAttachmentService;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.stereotype.Service;

import static nl.rivm.screenit.Constants.INLINE_ID_SO_LOGO_EMAIL;

@Service
@AllArgsConstructor
@Slf4j
public class MammaMergeMailAttachmentServiceImpl implements MammaMergeMailAttachmentService
{
	private final AfspraakIcalService afspraakIcalService;

	private final UploadDocumentService uploadDocumentService;

	@Override
	public List<MailAttachmentDto> maakMailAttachmentsAan(MailMergeContext mailMergeContext, DigitaalBerichtTemplate template)
	{
		List<MailAttachmentDto> mailAttachmentList = new ArrayList<>();
		var mergefields = template.getType().getMergeFields();
		var bodyTekst = template.getBody();
		String searchString = "{" + MergeField.SO_LOGO_EMAIL.getFieldName() + "}";

		if (mergefields.contains(MergeField.SO_LOGO_EMAIL) && bodyTekst.contains(searchString))
		{
			var logo = voegSoLogoToe(mailMergeContext);
			logo.ifPresent(mailAttachmentList::add);
		}

		if (template.getType() == DigitaalBerichtTemplateType.MAMMA_AFSPRAAK_BEVESTIGING)
		{
			var agendaUitnodiging = maakIcalAttachment(mailMergeContext);
			agendaUitnodiging.ifPresent(mailAttachmentList::add);
		}

		return mailAttachmentList;
	}

	private Optional<MailAttachmentDto> maakIcalAttachment(MailMergeContext mailMergeContext)
	{
		var locatie = getLocatieVoorAgenda(mailMergeContext);
		var locatieOmschrijving = maakLocatieOmschrijvingString(mailMergeContext);
		var client = mailMergeContext.getClient();

		var startDatum = DateUtil.toLocalDateTime(
			client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak().getVanaf());
		var eindDatum = startDatum.plusMinutes(15);
		var ontvanger = client.getPersoon().getEmailadres();
		var icalAfspraakId = client.getId().toString();
		var onderwerp = "Afspraak bevolkingsonderzoek borstkanker";
		var contentAgenda = "Afspraak bevolkingsonderzoek borstkanker op locatie: \n"
			+ locatie + "\n"
			+ locatieOmschrijving + "\n"
			+ "Vergeet niet mee te nemen: \n\n"
			+ " - Geldig identiteitsbewijs (paspoort, identiteitskaart of rijbewijs).\n"
			+ " - Uitnodigingsbrief (vul vooraf de achterkant in).\n";

		var calendar = afspraakIcalService.maakIcalEventEncoded(startDatum, eindDatum, ontvanger, locatie, icalAfspraakId, onderwerp, contentAgenda);

		return calendar.map(this::maakKalenderAttachmentDto);
	}

	private MailAttachmentDto maakKalenderAttachmentDto(String calendar)
	{
		var mailAttachment = new MailAttachmentDto();
		mailAttachment.setContent(calendar);
		mailAttachment.setContentType("text/calendar; method=PUBLISH");
		mailAttachment.setFileName("afspraak.ics");
		return mailAttachment;
	}

	private String getLocatieVoorAgenda(MailMergeContext mailMergeContext)
	{
		var straatnaam = MergeField.MAMMA_SP_STRAATNAAM.getFieldValue(mailMergeContext);
		var huisnummer = MergeField.MAMMA_SP_HUISNUMMER.getFieldValue(mailMergeContext);
		var postcode = MergeField.MAMMA_SP_POSTCODE.getFieldValue(mailMergeContext);
		var plaats = MergeField.MAMMA_SP_PLAATS.getFieldValue(mailMergeContext);

		return maakLocatieVoorAgenda(straatnaam, huisnummer, postcode, plaats);

	}

	private String maakLocatieVoorAgenda(Object straatnaam, Object huisnummer, Object postcode, Object plaats)
	{
		var stringBuilder = new StringBuilder();

		stringBuilder.append(straatnaam);
		stringBuilder.append(" ");

		if (huisnummer != null)
		{
			stringBuilder.append(huisnummer);
			stringBuilder.append(" ");
		}

		stringBuilder.append(postcode);
		stringBuilder.append(" ");
		stringBuilder.append(plaats);

		return stringBuilder.toString();
	}

	private String maakLocatieOmschrijvingString(MailMergeContext mailMergeContext)
	{
		var valueLocatieOmschrijving = MergeField.MAMMA_SP_LOC_OMSCHRIJVING.getFieldValue(mailMergeContext);

		return valueLocatieOmschrijving != null ? valueLocatieOmschrijving.toString() : "";

	}

	private Optional<MailAttachmentDto> voegSoLogoToe(MailMergeContext mailMergeContext)
	{
		var logoBrief = mailMergeContext.getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie().getLogoBrief();
		var logoBestand = uploadDocumentService.load(logoBrief);

		try (InputStream inputStream = new FileInputStream(logoBestand))
		{
			var encodedImage = Base64.getEncoder().encodeToString(inputStream.readAllBytes());
			MailAttachmentDto mailAttachment = new MailAttachmentDto();
			mailAttachment.setContent(encodedImage);
			mailAttachment.setContentType("image/jpg");
			mailAttachment.setInlineContentId(INLINE_ID_SO_LOGO_EMAIL);
			mailAttachment.setFileName("logo.jpg");
			return Optional.of(mailAttachment);
		}
		catch (Exception e)
		{
			LOG.error("bvo logo omzetten naar bytes mislukt " + e.getMessage());
		}
		return Optional.empty();
	}
}
