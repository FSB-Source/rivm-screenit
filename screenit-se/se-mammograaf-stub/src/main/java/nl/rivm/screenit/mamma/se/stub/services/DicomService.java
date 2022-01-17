package nl.rivm.screenit.mamma.se.stub.services;

/*-
 * ========================LICENSE_START=================================
 * se-mammograaf-stub
 * %%
 * Copyright (C) 2017 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.security.GeneralSecurityException;

import org.dcm4che3.data.Attributes;
import org.dcm4che3.net.IncompatibleConnectionException;

public interface DicomService
{
	void queryWorklist(Attributes worklistRequest) throws IOException, InterruptedException, GeneralSecurityException, IncompatibleConnectionException;

	void sendMppsCreate(Attributes mppsMessage, boolean foutMelding) throws Exception;

	void sendMppsUpdate(Attributes mppsMessage, boolean foutMelding) throws Exception;

	Attributes loadDicomResource(String xmlFileName) throws Exception;

	Attributes getResults();

	String getCurrentMppsUid();

	String getAETitle();
}
