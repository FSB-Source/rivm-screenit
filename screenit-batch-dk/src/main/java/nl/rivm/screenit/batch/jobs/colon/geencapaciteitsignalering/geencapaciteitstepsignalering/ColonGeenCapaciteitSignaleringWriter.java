package nl.rivm.screenit.batch.jobs.colon.geencapaciteitsignalering.geencapaciteitstepsignalering;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.DigitaalBerichtTemplateType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.DigitaalBerichtTemplateService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.colon.ColonIntakelocatieService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class ColonGeenCapaciteitSignaleringWriter extends BaseWriter<ColonIntakelocatie>
{
	private final MailService mailService;

	private final DigitaalBerichtTemplateService digitaalBerichtTemplateService;

	private final LogService logService;

	private final ColonIntakelocatieService intakelocatieService;

	@Override
	protected void write(ColonIntakelocatie intakelocatie)
	{
		var emails = getIntakelocatieSignaleringEmails(intakelocatie);
		var signaleringstermijnTekst = intakelocatieService.getSignaleringstermijnTekst();

		if (!emails.isEmpty())
		{
			var digitaalBericht = digitaalBerichtTemplateService.maakDigitaalBericht(DigitaalBerichtTemplateType.COLON_ROOSTER_GEEN_CAPACITEIT_SIGNALERING, intakelocatie);

			emails.forEach(email ->
			{
				mailService.queueMailAanProfessional(email, digitaalBericht.getSubject(), digitaalBericht.getBody());

				var logEvent = new LogEvent(String.format("Er is een mail verstuurd naar %s (%s) vanwege geen opgegeven capaciteit in termijn %s.", intakelocatie.getNaam(), email,
					signaleringstermijnTekst));
				logService.logGebeurtenis(LogGebeurtenis.COLON_ROOSTER_GEEN_CAPACITEIT_MAIL_VERSTUURD, logEvent, Bevolkingsonderzoek.COLON);
			});
		}
	}

	private List<String> getIntakelocatieSignaleringEmails(ColonIntakelocatie intakelocatie)
	{
		if (intakelocatie.getEmailSignaleringIntakelocatie() == null)
		{
			return List.of();
		}

		return Arrays.stream(intakelocatie.getEmailSignaleringIntakelocatie().split("[,;]+")).map(String::trim).collect(Collectors.toList());
	}
}
