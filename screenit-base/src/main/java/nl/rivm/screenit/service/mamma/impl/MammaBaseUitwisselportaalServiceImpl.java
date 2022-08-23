package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.mamma.MammaBaseUitwisselportaalDao;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenPoging;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseIlmService;
import nl.rivm.screenit.service.mamma.MammaBaseUitwisselportaalService;
import nl.rivm.screenit.service.mamma.MammaBaseVerslagService;
import nl.rivm.screenit.util.ZipUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.ImmutableMap;

@Slf4j

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseUitwisselportaalServiceImpl implements MammaBaseUitwisselportaalService
{
	@Autowired
	private LogService logService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseUitwisselportaalDao baseUitwisselportaalDao;

	@Autowired
	private MammaBaseVerslagService baseVerslagService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private BerichtToBatchService berichtToBatchService;

	@Autowired
	@Lazy
	private MammaBaseIlmService baseIlmService;

	@Override
	public void kopieerVerslagPdfNaarDownloadVerzoekMap(MammaDownloadOnderzoek downloadOnderzoek)
	{
		try
		{
			LOG.info("Start ophalen versalg voor " + downloadOnderzoek.getId());
			MammaBeoordeling beoordeling = downloadOnderzoek.getOnderzoek().getLaatsteBeoordeling();
			File verslagFile = baseVerslagService.getVerslagFile(beoordeling);
			if (verslagFile != null)
			{
				String onderzoekPath = getOnderzoekRootPath(downloadOnderzoek);
				File targetLocation = new File(onderzoekPath, "verslag.pdf");
				FileUtils.copyFile(verslagFile, targetLocation);
				if (beoordeling.getStatus() == MammaBeoordelingStatus.UITSLAG_GUNSTIG)
				{
					FileUtils.deleteQuietly(verslagFile);
				}
				LOG.info("Einde ophalen verslag voor " + downloadOnderzoek.getId());
			}
			else
			{
				LOG.warn("Geen verslag kunnen vinden voor beoordeling in download onderzoek: " + downloadOnderzoek.getId());
			}
		}
		catch (Exception e)
		{
			LOG.error("Fout bij kopieren beoordelingverslag naar verzoek map", e);
		}
	}

	@Override
	public void kopieerDicomBestandNaarDownloadVerzoekMap(File dicomFile, String seriesNumber, MammaDownloadOnderzoek downloadOnderzoek)
	{
		String onderzoekpath = getOnderzoekRootPath(downloadOnderzoek);
		File beeldDir = new File(
			onderzoekpath + File.separator + "SRS" + StringUtils.leftPad(seriesNumber, 5, "0"));
		try
		{
			FileUtils.copyFileToDirectory(dicomFile, beeldDir);
			LOG.info("Beeld naar " + beeldDir.getPath() + " gekopieerd.");
		}
		catch (IOException e)
		{
			LOG.error("Fout bij kopieren beeld naar verzoek map", e);
		}
	}

	@Override
	public String getOnderzoekRootPath(MammaDownloadOnderzoek downloadOnderzoek)
	{
		return getVerzoekRootPath(downloadOnderzoek.getVerzoek()) + File.separator
			+ new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDDHHMMSS).format(downloadOnderzoek.getOnderzoek().getCreatieDatum());
	}

	private String getVerzoekRootPath(MammaDownloadOnderzoekenVerzoek verzoek)
	{
		UploadDocument zipBestand = verzoek.getZipBestand();
		File zipFile = uploadDocumentService.load(zipBestand);
		String verzoekRootPath = zipFile.getParent() + File.separator + zipBestand.getId();
		return verzoekRootPath;
	}

	@Override
	public void zetFilesInZip(MammaDownloadOnderzoekenVerzoek verzoek) throws IOException
	{
		File zipBestand = uploadDocumentService.load(verzoek.getZipBestand());
		try
		{
			String verzoekRootPath = getVerzoekRootPath(verzoek);
			ZipUtil.zipFileOrDirectory(verzoekRootPath, zipBestand.getPath(), true);
			FileUtils.deleteDirectory(new File(verzoekRootPath));
		}
		catch (IOException e)
		{
			LOG.error("Fout bij maken van zip of verwijderen verzoek map", e);
			throw e;
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderDownloadVerzoeken(MammaDossier dossier)
	{
		List<MammaDownloadOnderzoekenVerzoek> verzoeken = baseUitwisselportaalDao.getDownloadVerzoeken(dossier);

		for (MammaDownloadOnderzoekenVerzoek verzoek : verzoeken)
		{
			uploadDocumentService.delete(verzoek.getZipBestand(), true);
			hibernateService.delete(verzoek);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderDownloadVerzoeken(MammaScreeningRonde screeningRonde)
	{
		List<MammaDownloadOnderzoekenVerzoek> verzoeken = baseUitwisselportaalDao.getDownloadVerzoeken(screeningRonde);

		for (MammaDownloadOnderzoekenVerzoek verzoek : verzoeken)
		{
			uploadDocumentService.delete(verzoek.getZipBestand(), true);
			hibernateService.delete(verzoek);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderUploadVerzoeken(MammaScreeningRonde ronde)
	{
		List<MammaUploadBeeldenVerzoek> verzoeken = ronde.getUploadBeeldenVerzoeken();
		for (MammaUploadBeeldenVerzoek verzoek : verzoeken)
		{
			for (MammaUploadBeeldenPoging uploadPoging : verzoek.getUploadPogingen())
			{
				verwijderBeeldenUploadPoging(uploadPoging);
			}
			hibernateService.delete(verzoek);
		}
		ronde.getUploadBeeldenVerzoeken().clear();
	}

	private void verwijderBeeldenUploadPoging(MammaUploadBeeldenPoging uploadBeeldenPoging)
	{
		for (UploadDocument bestand : uploadBeeldenPoging.getBestanden())
		{
			uploadDocumentService.delete(bestand, true);
		}
		uploadBeeldenPoging.getBestanden().clear();

		if (MammaMammografieIlmStatus.beeldenMogelijkAanwezig(uploadBeeldenPoging.getIlmStatus()))
		{
			berichtToBatchService.queueMammaUploadBeeldenHL7v24BerichtUitgaand(uploadBeeldenPoging, MammaHL7v24ORMBerichtStatus.GOINGTODELETE, null);
			berichtToBatchService.queueMammaUploadBeeldenHL7v24BerichtUitgaand(uploadBeeldenPoging, MammaHL7v24ORMBerichtStatus.DELETE, null);
			uploadBeeldenPoging.setIlmStatus(MammaMammografieIlmStatus.TE_VERWIJDEREN);
			uploadBeeldenPoging.setIlmStatusDatum(dateSupplier.getDate());
			baseIlmService.maakIlmBezwaarPoging(uploadBeeldenPoging.getUploadBeeldenVerzoek().getScreeningRonde().getDossier(), uploadBeeldenPoging.getAccessionNumber(), true);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderBeelden(MammaUploadBeeldenPoging uploadBeeldenPoging)
	{
		setIlmStatus(uploadBeeldenPoging, MammaMammografieIlmStatus.TE_VERWIJDEREN);
		berichtToBatchService.queueMammaUploadBeeldenHL7v24BerichtUitgaand(uploadBeeldenPoging, MammaHL7v24ORMBerichtStatus.DELETE, null);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void setIlmStatus(MammaUploadBeeldenPoging uploadBeeldenPoging, MammaMammografieIlmStatus ilmStatus)
	{
		MammaUploadBeeldenVerzoek uploadBeeldenVerzoek = uploadBeeldenPoging.getUploadBeeldenVerzoek();
		if (ilmStatus == MammaMammografieIlmStatus.VERWIJDERD)
		{
			uploadBeeldenVerzoek.setConclusieBirads(null);
			uploadBeeldenVerzoek.setConclusieEersteUitslagRadiologie(null);
		}
		uploadBeeldenPoging.setIlmStatus(ilmStatus);
		uploadBeeldenPoging.setIlmStatusDatum(dateSupplier.getDate());
		hibernateService.saveOrUpdateAll(uploadBeeldenPoging, uploadBeeldenVerzoek);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean forceerUploadPogingIlmStatus(long accessionNumber, MammaMammografieIlmStatus status, Account account)
	{
		MammaUploadBeeldenPoging uploadBeeldenPoging = getUploadPoging(accessionNumber);
		if (status != uploadBeeldenPoging.getIlmStatus())
		{
			setIlmStatus(uploadBeeldenPoging, MammaMammografieIlmStatus.VERWIJDERD);
			String melding = String.format("AccessionNumber: %d, status: %s, isBezwaar: %b, isUpload: %b", accessionNumber, status.toString(), false, true);
			LOG.info(melding);
			Client client = uploadBeeldenPoging.getUploadBeeldenVerzoek().getScreeningRonde().getDossier().getClient();
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_ILM_STATUS_GEFORCEERD, account, client, melding, Bevolkingsonderzoek.MAMMA);
			return true;
		}
		return false;
	}

	@Override
	public MammaUploadBeeldenPoging getUploadPoging(Long accessionNumber)
	{
		return hibernateService.getUniqueByParameters(MammaUploadBeeldenPoging.class, ImmutableMap.of("accessionNumber", accessionNumber));
	}
}
