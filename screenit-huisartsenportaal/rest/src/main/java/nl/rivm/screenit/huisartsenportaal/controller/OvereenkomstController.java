package nl.rivm.screenit.huisartsenportaal.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.huisartsenportaal.model.Overeenkomst;
import nl.rivm.screenit.huisartsenportaal.service.OvereenkomstService;

import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.FileSystemResource;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.aspose.words.Document;
import com.aspose.words.PdfSaveOptions;

@RestController
@RequestMapping("overeenkomst")
@PreAuthorize("isAuthenticated()")
@Slf4j
public class OvereenkomstController extends BaseController
{
	private PdfSaveOptions saveOptions;

	@Autowired
	private OvereenkomstService overeenkomstService;

	@Value("${app.filestoreLocatie}")
	private String filestoreLocatie;

	@RequestMapping(method = RequestMethod.GET, produces = "application/pdf")
	public FileSystemResource getOvereenkomst()
	{
		Overeenkomst overeenkomst = overeenkomstService.geefLaatsteOvereenkomst();
		if (overeenkomst != null)
		{
			try
			{
				File overeenkomstPdf = new File(
					System.getProperty("java.io.tmpdir") + File.separator + overeenkomst.getFileName() + "_" + overeenkomst.getHuisartsportaalId() + ".pdf");

				if (!overeenkomstPdf.exists())
				{
					File overeenkomstDocx = new File(filestoreLocatie + overeenkomst.getPath());
					overeenkomstPdf = File.createTempFile(overeenkomst.getFileName() + "_" + overeenkomst.getHuisartsportaalId(), ".pdf");
					byte[] overeenkomstBytes = FileUtils.readFileToByteArray(overeenkomstDocx);

					try (var overeenkomstInputStream = new ByteArrayInputStream(overeenkomstBytes); var output = new FileOutputStream(overeenkomstPdf))
					{
						var document = new Document(overeenkomstInputStream);
						document.save(output, getSaveOptions());
					}
				}
				return new FileSystemResource(overeenkomstPdf);
			}
			catch (Exception e)
			{
				LOG.error("Fout bij laden van overeenkomst", e);
			}
		}
		return null;
	}

	private PdfSaveOptions getSaveOptions()
	{
		if (saveOptions == null)
		{
			saveOptions = new PdfSaveOptions();
			saveOptions.setUseAntiAliasing(false);
			saveOptions.setUseHighQualityRendering(true);
			saveOptions.getDownsampleOptions().setResolution(300);
		}
		return saveOptions;
	}
}
