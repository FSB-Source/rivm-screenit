package nl.rivm.screenit.batch.service.impl;

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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.zip.ZipOutputStream;

import javax.annotation.PostConstruct;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.MammaVerzamelDownloadOnderzoekDataService;
import nl.rivm.screenit.batch.service.MammaVerzamelOnderzoekDataService;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseUitwisselportaalService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import com.google.common.collect.ImmutableMap;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class MammaVerzamelDownloadOnderzoekDataServiceImpl implements MammaVerzamelDownloadOnderzoekDataService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private SessionFactory sessionFactory;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private MammaVerzamelOnderzoekDataService verzamelOnderzoekDataService;

	@Autowired
	private MammaBaseUitwisselportaalService uitwisselPortaalservice;

	@Autowired
	private BerichtToBatchService berichtToBatchService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@PostConstruct
	public void init()
	{
		berichtToBatchService.queueMammaVerzamelOnderzoeksDataBericht();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public List<MammaDownloadOnderzoekenVerzoek> getAlleVerzamelDownloadOnderzoekDataVerzoeken()
	{
		return hibernateService.getByParameters(MammaDownloadOnderzoekenVerzoek.class, ImmutableMap.of("status", BestandStatus.NOG_TE_VERWERKEN));
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public void verzamelOnderzoekData(MammaDownloadOnderzoekenVerzoek verzoek)
	{
		Session session = null;
		boolean success = true;
		try
		{
			session = sessionFactory.openSession();
			TransactionSynchronizationManager.bindResource(sessionFactory, new SessionHolder(session));
			verzoek = hibernateService.getBoundObject(verzoek);
			LOG.info("Start verzamelen van beelden en verslag voor verzoek " + verzoek.getId());
			verzoek.setGewijzigdOp(dateSupplier.getDate());
			verzoek.setStatus(BestandStatus.BEZIG_MET_VERWERKEN);
			hibernateService.saveOrUpdate(verzoek);

			for (MammaDownloadOnderzoek onderzoek : verzoek.getOnderzoeken())
			{
				Files.createDirectories(Paths.get(uitwisselPortaalservice.getOnderzoekRootPath(onderzoek)));
				onderzoek.setStatus(BestandStatus.BEZIG_MET_VERWERKEN);
				hibernateService.saveOrUpdate(onderzoek);
				success &= verzamelOnderzoekDataService.addBeeldenAanVerzoek(onderzoek);
				verzamelOnderzoekDataService.addVerslagAanVerzoek(onderzoek);
			}
			createEmptyZipFile(verzoek);
			uitwisselPortaalservice.zetFilesInZip(verzoek);
		}
		catch (Exception e)
		{
			success = false;
			LOG.error("Fout bij verzamelen van beelden of verslagen: ", e);
		}
		finally
		{
			if (session != null)
			{
				if (success)
				{
					verzoek.setStatus(BestandStatus.VERWERKT);
				}
				else
				{
					verzoek.setStatus(BestandStatus.CRASH);
				}
				hibernateService.saveOrUpdate(verzoek);
				LOG.info("Klaar met verzamelen van beelden en verslag voor verzoek " + verzoek.getId());
				TransactionSynchronizationManager.unbindResource(sessionFactory);
				session.close();
			}
		}
	}

	private void createEmptyZipFile(MammaDownloadOnderzoekenVerzoek verzoek) throws IOException
	{
		File f = File.createTempFile("onderzoekData", ".zip");
		try (ZipOutputStream out = new ZipOutputStream(new FileOutputStream(f)))
		{
			out.closeEntry();
		}
		UploadDocument document = verzoek.getZipBestand();
		document.setFile(f);

		uploadDocumentService.update(document);
	}

}
