package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.repository.cervix.CervixFoutHL7v2BerichtRepository;
import nl.rivm.screenit.service.BaseClientContactService;
import nl.rivm.screenit.service.BaseDossierService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixBaseDossierService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.REQUIRED)
public class CervixBaseDossierServiceImpl implements CervixBaseDossierService
{
	private final HibernateService hibernateService;

	private final CervixBaseScreeningrondeService baseScreeningrondeService;

	private final CervixFoutHL7v2BerichtRepository foutHL7v2BerichtRepository;

	private final BaseClientContactService baseClientContactService;

	private final BaseDossierService baseDossierService;

	private final ClientService clientService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public void maakDossierLeeg(CervixDossier dossier)
	{
		Client client = dossier.getClient();

		baseScreeningrondeService.verwijderScreeningRondes(dossier);

		baseDossierService.verwijderNietLaatsteDefinitieveAfmeldingenUitDossier(dossier);

		baseClientContactService.verwijderClientContacten(client, Bevolkingsonderzoek.CERVIX);

		verwijderCisHistorie(dossier.getCisHistorie());

		verwijderFoutHl7V2Berichten(client);

		opruimenDossier(dossier);

		hibernateService.saveOrUpdate(client);

		ProjectClient projectClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate(), false);
		if (projectClient != null)
		{
			clientService.projectClientInactiveren(projectClient, ProjectInactiefReden.VERWIJDERING_VAN_DOSSIER, Bevolkingsonderzoek.CERVIX);
		}
	}

	private void verwijderFoutHl7V2Berichten(Client client)
	{
		var foutBerichten = foutHL7v2BerichtRepository.findAllByClient(client);
		foutHL7v2BerichtRepository.deleteInBatch(foutBerichten);
	}

	private void opruimenDossier(CervixDossier dossier)
	{
		dossier.setInactiefVanaf(null);
		dossier.setInactiefTotMet(null);
		CervixBrief vooraankondigingsBrief = dossier.getVooraankondigingsBrief();
		if (vooraankondigingsBrief != null)
		{
			dossier.setVooraankondigingsBrief(null);
			hibernateService.delete(vooraankondigingsBrief);
		}

		if (DossierStatus.INACTIEF.equals(dossier.getStatus()) && Boolean.TRUE.equals(dossier.getAangemeld()))
		{
			dossier.setStatus(DossierStatus.ACTIEF);
		}
		hibernateService.saveOrUpdate(dossier);
	}

	private void verwijderCisHistorie(CervixCISHistorie cisHistorie)
	{
		if (cisHistorie != null)
		{
			hibernateService.deleteAll(cisHistorie.getCisHistorieRegels());
			cisHistorie.getDossier().setCisHistorie(null);
			hibernateService.delete(cisHistorie);
		}
	}
}
