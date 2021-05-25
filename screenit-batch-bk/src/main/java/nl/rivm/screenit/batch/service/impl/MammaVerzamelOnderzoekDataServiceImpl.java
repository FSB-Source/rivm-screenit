package nl.rivm.screenit.batch.service.impl;

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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.MammaVerzamelOnderzoekDataService;
import nl.rivm.screenit.batch.service.impl.dicom.DicomCMoveSCU;
import nl.rivm.screenit.batch.service.impl.dicom.DicomDir;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.mamma.DicomCMoveConfig;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.service.mamma.MammaBaseUitwisselportaalService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.dcm4che3.data.Attributes;
import org.dcm4che3.data.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaVerzamelOnderzoekDataServiceImpl implements MammaVerzamelOnderzoekDataService
{

	private static final Logger LOG = LoggerFactory.getLogger(MammaVerzamelOnderzoekDataServiceImpl.class);

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseUitwisselportaalService uitwisselPortaalservice;

	@Override
	public void addVerslagAanVerzoek(MammaDownloadOnderzoek onderzoek)
	{
		uitwisselPortaalservice.kopieerVerslagPdfNaarDownloadVerzoekMap(onderzoek);
	}

	@Override
	public boolean addBeeldenAanVerzoek(MammaDownloadOnderzoek downloadOnderzoek)
	{
		boolean success = true;

		String connectionString = preferenceService.getString(PreferenceKey.INTERNAL_MAMMA_IMS_DICOM_CMOVE_CONFIG.toString());
		DicomCMoveConfig moveConfig = DicomCMoveConfig.parse(connectionString);
		DicomCMoveSCU moveSCU = new DicomCMoveSCU();
		MammaOnderzoek onderzoek = downloadOnderzoek.getOnderzoek();
		Long downloadOnderzoekId = downloadOnderzoek.getId();
		Long uitnodigingsNr = onderzoek.getAfspraak().getUitnodiging().getScreeningRonde().getUitnodigingsNr();
		LOG.info("Start verzamelen van beelden voor download onderzoek " + downloadOnderzoekId + " (Creatiedatum " + onderzoek.getCreatieDatum() + ", uitnodigingsNr: "
			+ uitnodigingsNr + ")");
		success = moveSCU.retrieve(moveConfig, uitnodigingsNr);
		Attributes result = moveSCU.getResult();
		if (result != null)
		{
			int status = result.getInt(Tag.Status, 0);
			int nrOfCompeleteSuboperations = result.getInt(Tag.NumberOfCompletedSuboperations, 0);
			if (status > 0)
			{
				String errorComment = result.getString(Tag.ErrorComment);
				if (StringUtils.isNotBlank(errorComment))
				{
					LOG.error("Error from IMS: " + errorComment + " Download onderzoek: " + downloadOnderzoekId);
					downloadOnderzoek.setStatusMelding("Foutmelding ontvangen van IMS. Neem contact op met helpdesk.");
				}
				else
				{
					int nrOfFailedSuboperations = result.getInt(Tag.NumberOfFailedSuboperations, 0);
					int nrOfWarningSuboperations = result.getInt(Tag.NumberOfWarningSuboperations, 0);
					downloadOnderzoek
						.setStatusMelding("#" + (nrOfFailedSuboperations + nrOfWarningSuboperations) + " (E" + nrOfFailedSuboperations + "/W" + nrOfWarningSuboperations
							+ ") beelden van totaal #" + (nrOfFailedSuboperations + nrOfWarningSuboperations + nrOfCompeleteSuboperations) + " niet kunnen ophalen.");
					LOG.error("Status melding van IMS: " + downloadOnderzoek.getStatusMelding() + " Download onderzoek: " + downloadOnderzoekId);
				}
				downloadOnderzoek.setStatus(BestandStatus.CRASH);
				success = false;
			}
			else
			{
				downloadOnderzoek.setStatusMelding("#" + nrOfCompeleteSuboperations + " beelden opgehaald.");
				downloadOnderzoek.setStatus(BestandStatus.VERWERKT);
				LOG.info(downloadOnderzoek.getStatusMelding() + " Download onderzoek: " + downloadOnderzoekId);
				createDicomDirFile(uitwisselPortaalservice.getOnderzoekRootPath(downloadOnderzoek));
			}
		}
		else
		{
			downloadOnderzoek.setStatus(BestandStatus.CRASH);
			downloadOnderzoek.setStatusMelding("Geen reactie van IMS");
			LOG.error(downloadOnderzoek.getStatusMelding() + " Download onderzoek: " + downloadOnderzoekId);
			success = false;
		}
		hibernateService.saveOrUpdate(downloadOnderzoek);
		return success;
	}

	private void createDicomDirFile(String onderzoekRootPath)
	{
		DicomDir dicomDir = new DicomDir(onderzoekRootPath);
		try
		{
			dicomDir.create();
		}
		catch (Exception e)
		{
			LOG.error("Fout tijdens maken van DICOMDIR voor map " + onderzoekRootPath, e);
		}
		finally
		{
			dicomDir.close();
		}
	}

}
