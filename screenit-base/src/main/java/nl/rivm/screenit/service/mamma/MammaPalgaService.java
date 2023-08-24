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

import java.io.IOException;
import java.util.List;

import nl.rivm.screenit.dto.mamma.MammaPalgaCsvImportDto;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaExportConfig;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaGrondslag;

public interface MammaPalgaService
{
	List<Long> getClientenVoorPalga(MammaPalgaExportConfig exportConfig);

	void deleteExports(String naam, Account loggedInAccount);

	UploadDocument getExport();

	UploadDocument getImport();

	void saveOrUpdateExport(UploadDocument zipDocument) throws IOException;

	void saveOrUpdateImport(UploadDocument importDocument) throws IOException;

	String saveOrUpdateImportZip(UploadDocument importDocument, String wachtwoord);

	void deleteImports();

	MammaPalgaCsvImportMapping maakImportDtoMapping(String[] row);

	String verwerkImportDto(MammaPalgaCsvImportDto dto, MammaPalgaGrondslag grondslag) throws NoSuchFieldException;
}
