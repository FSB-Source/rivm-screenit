package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.dao.DashboardDao;
import nl.rivm.screenit.main.service.ClientenVerwijderenTestService;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens;
import nl.rivm.screenit.model.dashboard.DashboardLogRegel;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.logging.LoggingZoekCriteria;
import nl.rivm.screenit.model.logging.NieuweIFobtAanvraagLogEvent;
import nl.rivm.screenit.model.mamma.MammaDeelnamekans;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.cervix.CervixTestService;
import nl.rivm.screenit.service.colon.ColonTestService;
import nl.rivm.screenit.service.mamma.MammaBaseTestService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.ImmutableMap;

@Slf4j
@Service
@Transactional(propagation = Propagation.REQUIRED)
public class ClientenVerwijderenTestServiceImpl implements ClientenVerwijderenTestService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ColonTestService colonTestService;

	@Autowired
	private CervixTestService cervixTestService;

	@Autowired
	private MammaBaseTestService mammaBaseTestService;

	@Autowired
	private TestService testService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private ClientDao clientDao;

	@Autowired
	private LogService logService;

	@Autowired
	private DashboardDao dashboardDao;

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public String clientenVerwijderen(String bsns)
	{
		int verwijderdeClienten = 0;
		String[] bsnList = bsns.split(",");
		String result = "Succesvol";
		for (String bsn : bsnList)
		{
			try
			{
				if (StringUtils.isBlank(bsn) || bsn.trim().length() != 9)
				{
					continue;
				}
				bsn = bsn.trim();
				Client client = clientDao.getClientByBsn(bsn);
				if (client == null)
				{
					continue;
				}

				testService.projectenVerwijderen(client);

				cervixTestService.clientReset(client);
				colonTestService.clientReset(client);
				mammaBaseTestService.clientReset(client);

				testService.verwijderClientContacten(client, true, true, true);

				LoggingZoekCriteria loggingZoekCriteria = new LoggingZoekCriteria();
				loggingZoekCriteria.setBsnClient(bsn);
				List<LogRegel> logRegels = logService.getLogRegels(loggingZoekCriteria, -1, -1, new SortState<String>("logGebeurtenis", Boolean.TRUE));
				for (LogRegel logRegel : logRegels)
				{
					List<DashboardLogRegel> dashboardLogRegels = dashboardDao.getDashboardLogRegelMetLogRegel(logRegel);
					hibernateService.deleteAll(dashboardLogRegels);

					hibernateService.delete(logRegel);
				}

				List<NieuweIFobtAanvraagLogEvent> logEvents = hibernateService.getByParameters(NieuweIFobtAanvraagLogEvent.class, ImmutableMap.of("client", client));
				logEvents.forEach(le -> hibernateService.delete(le.getLogRegel()));

				for (BezwaarMoment bezwaarMoment : client.getBezwaarMomenten())
				{
					UploadDocument brief = bezwaarMoment.getBezwaarBrief();
					if (brief != null)
					{
						bezwaarMoment.setBezwaarBrief(null);
						hibernateService.saveOrUpdate(bezwaarMoment);
						uploadDocumentService.delete(brief, true);
					}
				}
				client.setLaatstVoltooideBezwaarMoment(null);

				hibernateService.deleteAll(client.getBezwaarMomenten());
				List<GbaVraag> gbaVragen = client.getGbaVragen();
				hibernateService.deleteAll(gbaVragen);

				List<ClientContact> clientContacten = client.getContacten();
				hibernateService.deleteAll(clientContacten);

				Long clientId = client.getId();
				hibernateService.executeSql("update screenit_revision_entity set client = null where client = " + clientId);

				if (client.getMammaDossier() != null)
				{
					MammaDeelnamekans deelnamekans = client.getMammaDossier().getDeelnamekans();
					if (deelnamekans != null)
					{
						hibernateService.delete(deelnamekans);
					}
				}

				List<OverdrachtPersoonsgegevens> overdrachtPersoonsgegevensLijst = new ArrayList<>(client.getOverdrachtPersoonsgegevensLijst());
				hibernateService.deleteAll(overdrachtPersoonsgegevensLijst);
				client.getOverdrachtPersoonsgegevensLijst().clear();

				List<AlgemeneBrief> overgeblevenBrieven = hibernateService.getByParameters(AlgemeneBrief.class, ImmutableMap.of("client", client));
				hibernateService.deleteAll(overgeblevenBrieven);

				hibernateService.delete(client);

				verwijderdeClienten++;
			}
			catch (Exception e)
			{
				result = "Fout bij verwijderen van client met BSN " + bsn;
				LOG.error("error bij bsn " + bsn, e);
			}
		}
		return result + ". #" + verwijderdeClienten + " clienten verwijderd.";
	}

}
