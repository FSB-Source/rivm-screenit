package nl.rivm.screenit.batch.jobs.colon.uitnodigingenversturen.versturenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.colon.uitnodigingenversturen.UitnodigingenVersturenConstants;
import nl.rivm.screenit.batch.jobs.colon.uitnodigingenversturen.UitnodigingenVersturenProjectGroepCounterHolder;
import nl.rivm.screenit.batch.jobs.uitnodigingenversturen.versturenstep.AbstractUitnodigingenVersturenTasklet;
import nl.rivm.screenit.dao.colon.ColonUitnodigingsDao;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonMergedBrieven;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class ColonUitnodigingenVersturenTasklet extends AbstractUitnodigingenVersturenTasklet<ColonUitnodiging>
{
	private static final Logger LOG = LoggerFactory.getLogger(ColonUitnodigingenVersturenTasklet.class);

	@Autowired
	private ColonUitnodigingsDao uitnodigingsDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private ColonUitnodigingService colonUitnodigingService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private InstellingService instellingService;

	@Override
	protected List<Long> getUitnodigingen()
	{
		return uitnodigingsDao.getTeVersturenUitnodigingen();
	}

	@Override
	protected FileStoreLocation getFileStoreLocation()
	{
		return FileStoreLocation.COLON_UITNODIGINGEN_INPAKCENTRUM;
	}

	@Override
	protected void logMislukt(Client client)
	{
		logService.logGebeurtenis(LogGebeurtenis.UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM_MISLUKT, client, Bevolkingsonderzoek.COLON);
	}

	@Override
	protected void logMislukt(Long uitnodigingId)
	{
		String melding = String.format("Fout bij het versturen van uitnodiging met technisch id: %s", uitnodigingId);
		logService.logGebeurtenis(LogGebeurtenis.UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM_MISLUKT, null, melding, Bevolkingsonderzoek.COLON);
	}

	@Override
	protected void logNietBereikbaar(LogEvent event)
	{
		logService.logGebeurtenis(LogGebeurtenis.INPAKCENTRUM_NIET_BEREIKBAAR, event, Bevolkingsonderzoek.COLON);
	}

	@Override
	protected String getBvoAfkorting()
	{
		return Bevolkingsonderzoek.COLON.getAfkorting();
	}

	@Override
	protected boolean geenUitzonderingGevonden(ColonUitnodiging uitnodiging)
	{
		Client client = getClientVanUitnodiging(uitnodiging);
		if (!AdresUtil.isVolledigAdresVoorInpakcentrum(client))
		{
			String melding = "De cliënt heeft een onvolledig adres, dit is geconstateerd bij het aanmaken. De volgende gegevens ontbreken: "
				+ AdresUtil.bepaalMissendeAdresgegevensString(AdresUtil.getAdres(client.getPersoon(), currentDateSupplier.getDateTime())) + ".";
			int dagen = simplePreferenceService.getInteger(PreferenceKey.INTERNAL_HERINNERINGSPERIODE_LOGREGEL_ONVOLLEDIG_ADRES.name());
			LOG.warn("clientId " + client.getId() + ": " + melding);
			if (logService.heeftBestaandeLogregelBinnenPeriode(Collections.singletonList(LogGebeurtenis.COLON_ADRES_ONVOLLEDIG_VOOR_INPAKCENTRUM), client.getPersoon().getBsn(), melding, dagen))
			{
				List<Instelling> dashboardOrganisaties = addLandelijkBeheerInstelling(new ArrayList<>());
				dashboardOrganisaties.addAll(clientService.getScreeningOrganisatieVan(client));
				logService.logGebeurtenis(LogGebeurtenis.COLON_ADRES_ONVOLLEDIG_VOOR_INPAKCENTRUM, dashboardOrganisaties, null, client, melding,
					Bevolkingsonderzoek.COLON);
			}
			return false;
		}
		return true;
	}

	@Override
	protected synchronized void updateCounts(ColonUitnodiging uitnodiging)
	{
		Long aantalVerstuurd = getExecutionContext().getLong(uitnodiging.getColonUitnodigingCategorie().name());
		getExecutionContext().put(uitnodiging.getColonUitnodigingCategorie().name(), aantalVerstuurd + 1);

		Client client = getClientVanUitnodiging(uitnodiging);
		ColonUitnodigingCategorie uitnodigingCategorie = uitnodiging.getColonUitnodigingCategorie();
		List<ProjectClient> projectClienten = ProjectUtil.getHuidigeProjectClienten(client, currentDateSupplier.getDate(), true);
		if ((uitnodigingCategorie.equals(ColonUitnodigingCategorie.U1) || uitnodigingCategorie.equals(ColonUitnodigingCategorie.U2)) && projectClienten.size() > 0)
		{
			List<UitnodigingenVersturenProjectGroepCounterHolder> projectGroepenCounters = (List<UitnodigingenVersturenProjectGroepCounterHolder>) getExecutionContext()
				.get(UitnodigingenVersturenConstants.PROJECTENCOUNTERS);
			for (ProjectClient projectClient : projectClienten)
			{
				boolean projectGroepInList = false;
				for (UitnodigingenVersturenProjectGroepCounterHolder projectCounterHolder : projectGroepenCounters)
				{
					if (projectCounterHolder.getProjectGroepId().equals(projectClient.getGroep().getId()))
					{
						projectCounterHolder.setAantalVerstuurd(projectCounterHolder.getAantalVerstuurd() + 1);
						projectGroepInList = true;
						break;
					}
				}
				if (!projectGroepInList)
				{
					projectGroepenCounters.add(new UitnodigingenVersturenProjectGroepCounterHolder(projectClient.getGroep().getId(), 1L));
				}
			}
		}
	}

	@Override
	protected void setMergeContext(ColonUitnodiging uitnodiging, MailMergeContext mailMergeContext)
	{
		mailMergeContext.setColonUitnodiging(uitnodiging);
	}

	@Override
	protected void setGegenereerd(ColonUitnodiging uitnodiging)
	{

	}

	@Override
	protected void setMergedBrieven(ColonUitnodiging uitnodiging, UploadDocument uploadDocument, BriefDefinitie briefDefinitie)
	{
		ColonMergedBrieven colonMergedBrieven = new ColonMergedBrieven();
		colonMergedBrieven.setMergedBrieven(uploadDocument);
		colonMergedBrieven.setCreatieDatum(currentDateSupplier.getDate());
		colonMergedBrieven.setBriefType(briefDefinitie.getBriefType());

		hibernateService.saveOrUpdateAll(colonMergedBrieven);
	}

	@Override
	protected BriefDefinitie getBriefDefinitie(ColonUitnodiging uitnodiging)
	{
		return colonUitnodigingService.getBriefType(uitnodiging);
	}

	@Override
	protected ColonUitnodiging getUitnodigingById(Long uitnodigingId)
	{
		return hibernateService.get(ColonUitnodiging.class, uitnodigingId);
	}

	@Override
	protected void setUitnodigingVersturenTijd(List<Long> uitnodigingIds)
	{
		String uitnodigingUpdate = "UPDATE colon.colon_uitnodiging SET verstuurd_datum = :datum WHERE id in ( :uitnodigingIds )";
		hibernateService.getHibernateSession().createNativeQuery(uitnodigingUpdate)
			.setParameter("datum", currentDateSupplier.getDate())
			.setParameter("uitnodigingIds", uitnodigingIds)
			.executeUpdate();
	}

	private Client getClientVanUitnodiging(ColonUitnodiging uitnodiging)
	{
		return uitnodiging.getScreeningRonde().getDossier().getClient();
	}

	private List<Instelling> addLandelijkBeheerInstelling(List<Instelling> list)
	{
		list.addAll(instellingService.getActieveInstellingen(Rivm.class));
		return list;
	}
}
