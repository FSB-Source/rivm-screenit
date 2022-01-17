package nl.rivm.screenit.main.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.IOException;

import nl.rivm.screenit.model.berichten.ScreenITResponseV24MessageWrapper;
import nl.rivm.screenit.model.berichten.ScreenITResponseV251MessageWrapper;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.llp.LLPException;

public interface HL7TestMessageService
{

	ScreenITResponseV251MessageWrapper verstuurIFobtTestBericht(String message) throws LLPException, IOException, HL7Exception;

	ScreenITResponseV24MessageWrapper verstuurORMTestBericht(String message) throws LLPException, IOException, HL7Exception;

	ScreenITResponseV24MessageWrapper verstuurADTTestBericht(String message) throws LLPException, IOException, HL7Exception;

	ScreenITResponseV24MessageWrapper verstuurORMTestBerichtNaarScreenIT(String message) throws LLPException, IOException, HL7Exception;

	ScreenITResponseV24MessageWrapper verstuurILMTestBerichtNaarScreenIT(String message) throws LLPException, IOException, HL7Exception;
}
