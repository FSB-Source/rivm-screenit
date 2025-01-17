package nl.rivm.screenit.service;

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

import java.util.Optional;

import nl.rivm.screenit.dto.alg.client.contact.DigitaalBerichtDTO;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DigitaalBerichtTemplate;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.enums.DigitaalBerichtTemplateType;

public interface DigitaalBerichtTemplateService
{
	Optional<DigitaalBerichtTemplate> findDigitaalBerichtTemplate(DigitaalBerichtTemplateType type);

	void saveOrUpdateDigitaalBerichtTemplate(DigitaalBerichtTemplate template, Account account, String berichtTemplateNaam);

	DigitaalBerichtDTO maakDigitaalBericht(DigitaalBerichtTemplateType type, Client client);

	DigitaalBerichtDTO maakDigitaalBericht(DigitaalBerichtTemplateType type, ColonIntakelocatie intakelocatie);
}
