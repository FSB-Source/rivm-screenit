package nl.rivm.screenit.service.mamma;

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

import java.io.File;
import java.io.IOException;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenPoging;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;

public interface MammaBaseUitwisselportaalService
{

	void kopieerVerslagPdfNaarDownloadVerzoekMap(MammaDownloadOnderzoek downloadOnderzoek);

	void kopieerDicomBestandNaarDownloadVerzoekMap(File dicomTempFile, String seriesNumber, MammaDownloadOnderzoek downloadOnderzoek);

	void zetFilesInZip(MammaDownloadOnderzoekenVerzoek verzoek) throws IOException;

	String getOnderzoekRootPath(MammaDownloadOnderzoek downloadOnderzoek);

	void verwijderDownloadVerzoeken(MammaDossier dossier);

	void verwijderDownloadVerzoeken(MammaScreeningRonde screeningRonde);

	void verwijderUploadVerzoeken(MammaScreeningRonde screeningRonde);

	void verwijderBeelden(MammaUploadBeeldenPoging uploadBeeldenPoging);

	void setIlmStatus(MammaUploadBeeldenPoging uploadBeeldenPoging, MammaMammografieIlmStatus ilmStatus);

	boolean forceerUploadPogingIlmStatus(long accessionNumber, MammaMammografieIlmStatus status, Account account);

	MammaUploadBeeldenPoging getUploadPoging(Long accessionNumber);
}
