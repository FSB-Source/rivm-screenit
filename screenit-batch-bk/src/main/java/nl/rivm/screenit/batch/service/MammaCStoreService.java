package nl.rivm.screenit.batch.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.util.List;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenPoging;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;
import nl.rivm.screenit.model.mamma.berichten.MammaIMSBericht;

import ca.uhn.hl7v2.HL7Exception;

public interface MammaCStoreService
{
    void beeldenOntvangenUploadPoging(MammaUploadBeeldenPoging uploadBeeldenPoging) throws HL7Exception;

    void beeldenVerwijderdUploadVerzoek(MammaUploadBeeldenPoging uploadBeeldenPoging, MammaIMSBericht bericht, Client client, boolean error);

    List<MammaUploadBeeldenVerzoek> getOpenstaandeUploadVerzoeken();

    void verstuurBeelden(MammaUploadBeeldenVerzoek uploadBeeldenVerzoek, String sopClasses);
}
