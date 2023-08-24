package nl.rivm.screenit.batch.jobs.cervix.uitnodigingenversturen.versturenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.dm_ict.photo._358.MERGEDATA.UITNODIGING;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.dao.CervixUitnodigingsDao;
import nl.rivm.screenit.batch.jobs.cervix.uitnodigingenversturen.ProjectCounterHolder;
import nl.rivm.screenit.batch.jobs.cervix.uitnodigingenversturen.ZasUitnodigingenVersturenConstants;
import nl.rivm.screenit.batch.jobs.uitnodigingenversturen.versturenstep.AbstractUitnodigingenVersturenTasklet;
import nl.rivm.screenit.dao.BaseBriefDao;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixMergedBrieven;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
@Slf4j
public class ZasUitnodigingenVersturenTasklet extends AbstractUitnodigingenVersturenTasklet<CervixUitnodiging>
{

	private static final String ZAS_TYPE_COMBI = "COMBI";

	private static final String ZAS_TYPE_STANDAARD = "STANDAARD";

	private final LogService logService;

	private final BaseBriefDao briefDao;

	private final CervixUitnodigingsDao uitnodigingsDao;

	private final HibernateService hibernateService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final ClientService clientService;

	private final InstellingService instellingService;

	private final SimplePreferenceService simplePreferenceService;

	@SuppressWarnings("unchecked")
	@Override
	protected List<Long> getUitnodigingen()
	{
		return uitnodigingsDao.getTeVersturenZasUitnodigingen();
	}

	@Override
	protected BriefDefinitie getBriefDefinitie(CervixUitnodiging uitnodiging)
	{
		return briefDao.getNieuwsteBriefDefinitie(uitnodiging.getBrief().getBriefType());
	}

	@Override
	protected void setMergedBrieven(CervixUitnodiging uitnodiging, UploadDocument uploadDocument, BriefDefinitie briefDefinitie)
	{
		var brief = uitnodiging.getBrief();
		var mergedBrieven = new CervixMergedBrieven();
		mergedBrieven.setMergedBrieven(uploadDocument);
		mergedBrieven.setCreatieDatum(currentDateSupplier.getDate());
		mergedBrieven.setBriefType(brief.getBriefType());
		brief.setMergedBrieven(mergedBrieven);
		brief.setBriefDefinitie(briefDefinitie);
		hibernateService.saveOrUpdateAll(mergedBrieven, brief);
	}

	@Override
	protected void setGegenereerd(CervixUitnodiging uitnodiging)
	{
		var cervixBrief = uitnodiging.getBrief();
		cervixBrief.setGegenereerd(true);
		hibernateService.saveOrUpdate(cervixBrief);
	}

	@Override
	protected void setMergeContext(CervixUitnodiging uitnodiging, MailMergeContext mailMergeContext)
	{
		if (uitnodiging.getGecombineerdeUitstrijkje() != null)
		{
			uitnodiging = uitnodiging.getGecombineerdeUitstrijkje();
		}
		mailMergeContext.setBrief(uitnodiging.getBrief());
		mailMergeContext.setCervixUitnodiging(uitnodiging);
	}

	@Override
	protected FileStoreLocation getFileStoreLocation()
	{
		return FileStoreLocation.CERVIX_ZAS_UITNODIGINGEN_INPAKCENTRUM;
	}

	@Override
	protected synchronized void updateCounts(CervixUitnodiging uitnodiging)
	{
		long aantalVerstuurd = getExecutionContext().getLong(uitnodiging.getBrief().getBriefType().name());
		getExecutionContext().put(uitnodiging.getBrief().getBriefType().name(), aantalVerstuurd + 1);

		updateProjectenCounts(uitnodiging);
	}

	private void updateProjectenCounts(CervixUitnodiging uitnodiging)
	{
		var client = uitnodiging.getScreeningRonde().getDossier().getClient();

		var projectClienten = ProjectUtil.getHuidigeProjectClienten(client, currentDateSupplier.getDate(), true);
		if (BriefType.getCervixZasBrieven().contains(uitnodiging.getBrief().getBriefType()) && client != null && client.getProjecten() != null && !projectClienten.isEmpty())
		{
			var projectCounterHolders = getProjectCounterHolders();
			for (var projectClient : projectClienten)
			{
				boolean projectInList = false;
				for (var projectCounterHolder : projectCounterHolders)
				{
					if (projectCounterHolder.getProjectGroepId().equals(projectClient.getGroep().getId()))
					{
						projectCounterHolder.setAantalVerstuurd(projectCounterHolder.getAantalVerstuurd() + 1);
						projectInList = true;
						break;
					}
				}
				if (!projectInList)
				{
					projectCounterHolders.add(new ProjectCounterHolder(projectClient.getGroep().getId(), 1L));
				}
			}
		}
	}

	@NotNull
	private List<ProjectCounterHolder> getProjectCounterHolders()
	{
		var projectCounterHolders = (List<ProjectCounterHolder>) getExecutionContext().get(ZasUitnodigingenVersturenConstants.PROJECTENCOUNTERS);
		if (projectCounterHolders == null)
		{
			projectCounterHolders = new ArrayList<>();
			getExecutionContext().put(ZasUitnodigingenVersturenConstants.PROJECTENCOUNTERS, projectCounterHolders);
		}
		return projectCounterHolders;
	}

	@Override
	protected void logMislukt(Client client)
	{
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_ZAS_UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM_MISLUKT, client, Bevolkingsonderzoek.CERVIX);
	}

	@Override
	protected void logMislukt(Long uitnodigingsId)
	{
		var melding = String.format("Fout bij het versturen van uitnodiging met uitnodigingsId: '%s'", uitnodigingsId);
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_ZAS_UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM_MISLUKT, null, melding, Bevolkingsonderzoek.COLON);
	}

	@Override
	protected void logNietBereikbaar(LogEvent event)
	{
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_ZAS_INPAKCENTRUM_NIET_BEREIKBAAR, event, Bevolkingsonderzoek.CERVIX);
	}

	@Override
	protected String getBvoAfkorting()
	{
		return Bevolkingsonderzoek.CERVIX.getAfkorting();
	}

	@Override
	protected boolean geenUitzonderingGevonden(CervixUitnodiging uitnodiging)
	{
		var melding = new StringBuilder();
		MergeField.getBmhkRetouradresMetMelding(uitnodiging, melding);
		var client = uitnodiging.getScreeningRonde().getDossier().getClient();
		if (melding.length() > 0)
		{
			melding.append("ZAS uitnodiging niet verstuurd naar inpakcentrum!");
			LOG.warn("client (id '{}'): {}", client.getId(), melding);
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_FOUT_RETOURADRES_BMHK_LAB, client, null, melding.toString(), Bevolkingsonderzoek.CERVIX);
			return false;
		}
		if (!AdresUtil.isVolledigAdresVoorInpakcentrum(client))
		{
			var onvolledigAdresMelding = "De cliënt heeft een onvolledig adres, dit is geconstateerd bij het aanmaken. De volgende gegevens ontbreken: "
				+ AdresUtil.bepaalMissendeAdresgegevensString(AdresUtil.getAdres(client.getPersoon(), currentDateSupplier.getLocalDate())) + ".";
			int dagen = simplePreferenceService.getInteger(PreferenceKey.INTERNAL_HERINNERINGSPERIODE_LOGREGEL_ONVOLLEDIG_ADRES.name());
			if (logService.heeftGeenBestaandeLogregelBinnenPeriode(Collections.singletonList(LogGebeurtenis.CERVIX_ADRES_ONVOLLEDIG_VOOR_INPAKCENTRUM),
				client.getPersoon().getBsn(),
				onvolledigAdresMelding, dagen))
			{
				melding.append(onvolledigAdresMelding);
				LOG.warn("client (id '{}'): {}", client.getId(), melding);
				var dashboardOrganisaties = addLandelijkBeheerInstelling(new ArrayList<>());
				dashboardOrganisaties.addAll(clientService.getScreeningOrganisatieVan(client));
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_ADRES_ONVOLLEDIG_VOOR_INPAKCENTRUM, dashboardOrganisaties, null, client, melding.toString(),
					Bevolkingsonderzoek.CERVIX);
			}

			return false;
		}

		return true;
	}

	@Override
	protected CervixUitnodiging getUitnodigingById(Long uitnodigingId)
	{
		return hibernateService.get(CervixUitnodiging.class, uitnodigingId);
	}

	@Override
	protected void setUitnodigingVersturenTijd(List<Long> uitnodigingIds)
	{
		var uitnodigingUpdate = "UPDATE cervix.uitnodiging SET verstuurd_datum = :datum WHERE id in ( :uitnodigingIds )";
		hibernateService.getHibernateSession().createNativeQuery(uitnodigingUpdate)
			.setParameter("datum", currentDateSupplier.getDate())
			.setParameter("uitnodigingIds", uitnodigingIds)
			.executeUpdate();
	}

	@Override
	protected void vulMetaData(UITNODIGING inpakcentrumUitnodiging, ProjectBriefActie briefActie, MailMergeContext mailMergeContext, int uitnodigingVolgnummer,
		UploadDocument uploadDocument)
	{
		super.vulMetaData(inpakcentrumUitnodiging, briefActie, mailMergeContext, uitnodigingVolgnummer, uploadDocument);
		var mergefieldContainer = inpakcentrumUitnodiging.getMERGEFIELDS().getMERGEFIELD();
		var uitnodiging = mailMergeContext.getCervixUitnodiging();
		addMergeFieldValue(mergefieldContainer, "_TYPE", uitnodiging.getGecombineerdeZas() != null ? ZAS_TYPE_COMBI : ZAS_TYPE_STANDAARD);
	}

	private List<Instelling> addLandelijkBeheerInstelling(List<Instelling> list)
	{
		list.addAll(instellingService.getActieveInstellingen(Rivm.class));
		return list;
	}
}
