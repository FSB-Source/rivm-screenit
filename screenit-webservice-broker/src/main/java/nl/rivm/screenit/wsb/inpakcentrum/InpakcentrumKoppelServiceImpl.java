package nl.rivm.screenit.wsb.inpakcentrum;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.io.StringReader;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.util.List;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.algemeen.KoppelData;
import nl.rivm.screenit.model.batch.BatchJob;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobStartParameter;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.JobService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.ws.inpakcentrum.InpakcentrumKoppelService;
import nl.rivm.screenit.ws.inpakcentrum.KoppelDataResponse;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import generated.KOPPELDATA;
import generated.KOPPELDATA.VERZONDENUITNODIGING;

@Service("InpakcentrumKoppelService_PortType")
@Transactional(propagation = Propagation.SUPPORTS)
@WebService(targetNamespace = "http:screenit.rivm.nl/", name = "InpakcentrumKoppelService")
public class InpakcentrumKoppelServiceImpl implements InpakcentrumKoppelService
{

	private static final Logger LOG = LoggerFactory.getLogger(InpakcentrumKoppelServiceImpl.class);

	@Autowired
	private JobService jobService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private LogService logService;

	@Override
	@WebResult(name = "return", targetNamespace = "")
	@RequestWrapper(localName = "submitKoppelData", targetNamespace = "http:screenit.rivm.nl/", className = "nl.rivm.screenit.ws.inpakcentrum.SubmitKoppelData")
	@WebMethod
	@ResponseWrapper(localName = "submitKoppelDataResponse", targetNamespace = "http:screenit.rivm.nl/", className = "nl.rivm.screenit.ws.inpakcentrum.SubmitKoppelDataResponse")
	public nl.rivm.screenit.ws.inpakcentrum.KoppelDataResponse submitKoppelData( //
		@WebParam(name = "koppelData", targetNamespace = "") nl.rivm.screenit.ws.inpakcentrum.KoppelData koppelData, //
		@WebParam(name = "onlyValidation", targetNamespace = "") boolean onlyValidation //
	)
	{

		Bevolkingsonderzoek bvo = null;
		String filename = "";
		if (koppelData != null && StringUtils.isNotBlank(koppelData.getFilename()))
		{
			filename = koppelData.getFilename();
			String[] filenameParts = koppelData.getFilename().split("_");
			if (filenameParts.length > 0)
			{
				for (Bevolkingsonderzoek bevolkingsonderzoek : Bevolkingsonderzoek.values())
				{
					if (bevolkingsonderzoek.getAfkorting().equals(filenameParts[0].toUpperCase()))
					{
						bvo = bevolkingsonderzoek;
						break;
					}
				}
			}
		}
		if (bvo == null)
		{
			LOG.warn("Er kon geen BVO bepaald worden uit de 'filename' waardoor standaard wordt gekozen voor COLON, filename: " + filename);
			bvo = Bevolkingsonderzoek.COLON;
		}

		if (Bevolkingsonderzoek.COLON.equals(bvo))
		{
			logService.logGebeurtenis(LogGebeurtenis.IFOBT_CONTROLE_KOPPELEN_GESTART, new LogEvent(), Bevolkingsonderzoek.COLON);
		}
		else if (Bevolkingsonderzoek.CERVIX.equals(bvo))
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_ZAS_CONTROLE_KOPPELEN_GESTART, new LogEvent(), Bevolkingsonderzoek.CERVIX);
		}

		nl.rivm.screenit.model.algemeen.KoppelData koppelData2 = new KoppelData();
		koppelData2.setXmlBericht(new String(koppelData.getData(), Charset.forName("UTF8")));
		koppelData2.setFilename(koppelData.getFilename());
		koppelData2.setOntvangen(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(koppelData2);

		LogEvent eindEvent = new LogEvent();
		eindEvent.setLevel(Level.ERROR);
		KoppelDataResponse koppelDataResponse = new KoppelDataResponse();
		koppelDataResponse.setSucces(false);
		List<String> foutmeldingen = koppelDataResponse.getFoutmeldingens();

		BatchJob batchJob = new BatchJob();
		if (Bevolkingsonderzoek.COLON.equals(bvo))
		{
			batchJob.setJobType(JobType.KOPPELDATA_VERWERKING);
		}
		if (Bevolkingsonderzoek.CERVIX.equals(bvo))
		{
			batchJob.setJobType(JobType.CERVIX_KOPPELDATA_VERWERKING);
		}

		try
		{
			JAXBContext jaxbContext = JAXBContext.newInstance(KOPPELDATA.class);
			Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
			String xmlString = koppelData2.getXmlBericht();
			List<VERZONDENUITNODIGING> koppeldata = ((KOPPELDATA) unmarshaller.unmarshal(new StringReader(xmlString))).getVERZONDENUITNODIGING();

			if (koppeldata.size() > 0)
			{
				koppelDataResponse.setSucces(true);
				koppelDataResponse.setAantalFouten(BigInteger.valueOf(0));
			}

			batchJob.getJobParameters().put(JobStartParameter.KOPPEL_XML.name(), koppelData2.getId());
			batchJob.getJobParameters().put(Constants.ALLEEN_VALIDATIE, onlyValidation);
			LOG.info("Geen fouten bij verwerking koppeldata, dus doorzetten naar batch voor verdere validatie.");
			jobService.startJob(batchJob, null);
		}
		catch (JAXBException e)
		{
			eindEvent.setMelding("Fout bij verwerken van koppel data: " + e.getMessage());
			LOG.error("Fout bij verwerken van koppel data door webservice ", e);
			foutmeldingen.add(eindEvent.getMelding());
			koppelDataResponse.setAantalFouten(BigInteger.valueOf(foutmeldingen.size()));
		}

		return koppelDataResponse;
	}
}
