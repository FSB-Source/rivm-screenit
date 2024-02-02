package nl.rivm.screenit.batch.jobs.mamma.uitwisselportaal.cleanup;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.io.File;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.io.FileUtils;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
@Slf4j
public class MammaDownloadedDataCleanUpWriter extends BaseWriter<MammaDownloadOnderzoekenVerzoek>
{
	private final UploadDocumentService uploadDocumentService;

	private final HibernateService hibernateService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void write(MammaDownloadOnderzoekenVerzoek item) throws Exception
	{
		var zipBestand = item.getZipBestand();

		if (zipBestand != null)
		{
			File file = uploadDocumentService.load(zipBestand);
			FileUtils.deleteQuietly(file);
		}

		item.setStatus(BestandStatus.VERWIJDERD);
		item.setGewijzigdOp(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(item);
		LOG.info("Download bestanden verwijderd van item: " + item.getId());
	}
}
