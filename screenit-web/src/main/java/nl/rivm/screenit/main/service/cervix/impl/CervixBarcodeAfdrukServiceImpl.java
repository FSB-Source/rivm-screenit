package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.cervix.CervixBarcodeAfdrukService;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.aspose.words.Document;

@Service
public class CervixBarcodeAfdrukServiceImpl implements CervixBarcodeAfdrukService
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixBarcodeAfdrukServiceImpl.class);

	@Autowired
	private AsposeService asposeService;

	@Autowired
	private BaseBriefService briefService;

	@Override
	public File saveBarcodeDocument(CervixUitnodiging uitnodiging)
	{
		File file = null;
		try
		{
			MailMergeContext context = new MailMergeContext();
			context.setCervixUitnodiging(uitnodiging);
			context.setClient(uitnodiging.getScreeningRonde().getDossier().getClient());
			File template = new File(getClass().getClassLoader().getResource("CervixUitnodigingsSticker.doc").toURI());
			byte[] templateBytes = FileUtils.readFileToByteArray(template);
			Document document = asposeService.processDocument(templateBytes, context);
			file = briefService.genereerPdf(document, "MonsterId", true);
		}
		catch (Exception e)
		{
			LOG.error("Error bij laden PDF barcode monster: {}", uitnodiging.getMonster().getMonsterId());
		}
		return file;
	}
}
