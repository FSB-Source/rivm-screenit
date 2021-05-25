package nl.rivm.screenit.batch.jobs.mamma.uitwisselportaal.cleanup;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaDownloadedDataCleanUpWriter extends BaseWriter<MammaDownloadOnderzoekenVerzoek>
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaDownloadedDataCleanUpWriter.class);

	@Autowired
	private FileService fileService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void write(MammaDownloadOnderzoekenVerzoek item) throws Exception
	{
		UploadDocument zipBestand = item.getZipBestand();

		if (zipBestand != null)
		{
			File file = fileService.load(zipBestand);
			FileUtils.deleteQuietly(file);
		}

		item.setStatus(BestandStatus.VERWIJDERD);
		item.setGewijzigdOp(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(item);
		LOG.info("Download bestanden verwijderd van item: " + item.getId());
	}
}
