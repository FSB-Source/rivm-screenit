package nl.rivm.screenit.batch.jobs.cervix.verrichtingen.cleanup;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;

@Slf4j
public class BetalingBestandenCleanUpWriter extends BaseWriter<CervixBetaalopdracht>
{
	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void write(CervixBetaalopdracht item) throws Exception
	{
		UploadDocument specificatie = item.getSepaSpecificatiePdf();
		UploadDocument sepa = item.getSepaDocument();

		if (specificatie != null)
		{
			item.setSepaSpecificatiePdf(null);
			uploadDocumentService.delete(specificatie, true);
		}
		if (sepa != null)
		{
			item.setSepaDocument(null);
			uploadDocumentService.delete(sepa, true);
		}
		item.setStatus(BestandStatus.VERWIJDERD);
		item.setStatusDatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(item);
		LOG.info("Betaling bestanden verwijderd van item: " + item.getId());
	}
}
