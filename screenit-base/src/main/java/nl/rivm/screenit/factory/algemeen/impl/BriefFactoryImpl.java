package nl.rivm.screenit.factory.algemeen.impl;

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

import java.util.Date;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.factory.algemeen.BriefFactory;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.repository.BaseJpaRepository;
import nl.rivm.screenit.repository.algemeen.AlgemeneBriefRepository;
import nl.rivm.screenit.repository.algemeen.BezwaarBriefRepository;
import nl.rivm.screenit.repository.algemeen.ProjectBriefRepository;
import nl.rivm.screenit.repository.cervix.CervixBriefRepository;
import nl.rivm.screenit.repository.cervix.CervixRegioBriefRepository;
import nl.rivm.screenit.repository.colon.ColonBriefRepository;
import nl.rivm.screenit.repository.mamma.MammaBriefRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.cervix.CervixRegioBriefSpecification;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.algemeen.BriefSpecification.heeftBriefTypeIn;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.isNietVervangen;
import static nl.rivm.screenit.specification.algemeen.ClientBriefSpecification.heeftOngegeneerdeBrieven;
import static nl.rivm.screenit.specification.cervix.CervixRegioBriefSpecification.heeftHuisarts;

@Slf4j
@Component
public class BriefFactoryImpl implements BriefFactory
{
	@Autowired
	private CervixRegioBriefRepository cervixRegioBriefRepository;

	@Autowired
	private AlgemeneBriefRepository algemeneBriefRepository;

	@Autowired
	private ProjectBriefRepository projectBriefRepository;

	@Autowired
	private BezwaarBriefRepository bezwaarBriefRepository;

	@Autowired
	private MammaBriefRepository mammaBriefRepository;

	@Autowired
	private CervixBriefRepository cervixBriefRepository;

	@Autowired
	private ColonBriefRepository colonBriefRepository;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public <B extends ClientBrief<?, ?, ?>> BaseJpaRepository<B> getBriefTypeRepository(Class<B> briefClass)
	{
		if (briefClass.isAssignableFrom(AlgemeneBrief.class))
		{
			return (BaseJpaRepository<B>) algemeneBriefRepository;
		}
		else if (briefClass.isAssignableFrom(CervixBrief.class))
		{
			return (BaseJpaRepository<B>) cervixBriefRepository;
		}
		else if (briefClass.isAssignableFrom(ProjectBrief.class))
		{
			return (BaseJpaRepository<B>) projectBriefRepository;
		}
		else if (briefClass.isAssignableFrom(BezwaarBrief.class))
		{
			return (BaseJpaRepository<B>) bezwaarBriefRepository;
		}
		else if (briefClass.isAssignableFrom(MammaBrief.class))
		{
			return (BaseJpaRepository<B>) mammaBriefRepository;
		}
		else if (briefClass.isAssignableFrom(ColonBrief.class))
		{
			return (BaseJpaRepository<B>) colonBriefRepository;
		}

		throw new IllegalArgumentException("Onbekend briefClass: " + briefClass.getName());
	}

	@Override
	@Transactional
	public BezwaarBrief maakBezwaarBrief(Client client, BriefType type, Date date)
	{
		return maakBezwaarBrief(client, type, date, false);
	}

	@Override
	@Transactional
	public BezwaarBrief maakBezwaarBrief(Client client, BriefType type, Date date, boolean vragenOmHandtekening)
	{
		vervangDubbeleAangemaakteBrieven(type, client, BezwaarBrief.class);
		var brief = BriefUtil.maakBezwaarBrief(client, type, date == null ? currentDateSupplier.getDate() : date, vragenOmHandtekening);
		hibernateService.saveOrUpdate(brief);
		return brief;
	}

	@Override
	@Transactional
	public AlgemeneBrief maakAlgemeneBrief(Client client, BriefType type)
	{
		vervangDubbeleAangemaakteBrieven(type, client, AlgemeneBrief.class);
		var brief = BriefUtil.maakAlgemeneBrief(client, type, currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(brief);
		return brief;
	}

	@Override
	@Transactional
	public <B extends ClientBrief<?, A, ?>, A extends Afmelding<?, ?, B>> B maakBvoBrief(A afmelding, BriefType type, Date creatieMoment, boolean vervangendeProjectBrief)
	{
		Client client = AfmeldingUtil.getClientFromAfmelding(afmelding);

		B brief = maakBvoBrief(client, type, creatieMoment, false, vervangendeProjectBrief);
		brief.setAfmelding(afmelding);
		afmelding.getBrieven().add(brief);

		hibernateService.saveOrUpdate(brief);
		hibernateService.saveOrUpdate(afmelding);
		return brief;
	}

	@Override
	@Transactional
	public <B extends ClientBrief<?, A, ?>, A extends Afmelding<?, ?, B>> B maakBvoBrief(A afmelding, BriefType type, Date creatieMoment)
	{
		return maakBvoBrief(afmelding, type, creatieMoment, false);
	}

	@Override
	@Transactional
	public <B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type)
	{
		return maakBvoBrief(ronde, type, null, false, false);
	}

	@Override
	@Transactional
	public <B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, boolean gegenereerd)
	{
		return maakBvoBrief(ronde, type, null, gegenereerd, false);
	}

	@Override
	@Transactional
	public <B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, Date creatieMoment)
	{
		return maakBvoBrief(ronde, type, creatieMoment, false, false);
	}

	@Override
	@Transactional
	public <B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, Date creatieMoment, boolean gegenereerd,
		boolean vervangendeProjectBrief)
	{
		B brief = maakBvoBrief(ronde.getDossier().getClient(), type, creatieMoment, gegenereerd, vervangendeProjectBrief);
		brief.setScreeningRonde(ronde);
		ronde.getBrieven().add(brief);
		ronde.setLaatsteBrief(brief);
		hibernateService.saveOrUpdate(brief);
		hibernateService.saveOrUpdate(ronde);
		return brief;
	}

	private <B extends ClientBrief<?, ?, ?>> B maakBvoBrief(Client client, BriefType type, Date creatieMoment, boolean gegenereerd, boolean vervangendeProjectBrief)
	{
		B brief = BriefUtil.maakBvoBrief(client, type, creatieMoment, gegenereerd);
		vervangDubbeleAangemaakteBrieven(type, client, brief.getClass());
		if (creatieMoment == null)
		{
			brief.setCreatieDatum(currentDateSupplier.getDate());
		}
		hibernateService.saveOrUpdate(brief);
		LOG.info("Brief klaargezet met type {} voor client (id: '{}')", type, client.getId());
		if (!vervangendeProjectBrief)
		{
			checkVoorProjectClient(brief, client);
		}
		return brief;
	}

	@Override
	@Transactional
	public CervixRegioBrief maakRegioBrief(ScreeningOrganisatie so, BriefType type, Date date, CervixHuisarts arts)
	{
		checkVoorDubbeleBrieven(type, arts);
		var brief = BriefUtil.maakRegioBrief(so, type, date, arts);
		if (date == null)
		{
			brief.setCreatieDatum(currentDateSupplier.getDate());
		}
		hibernateService.saveOrUpdate(brief);
		LOG.info("Brief klaargezet met type {} voor regio: {}", type, so.getNaam());
		return brief;
	}

	@Override
	@Transactional
	public ProjectBrief maakProjectBrief(ProjectClient projectClient, ProjectBriefActie actie, ProjectBrief origineleBrief)
	{
		var projectBrief = BriefUtil.maakProjectBrief(projectClient, actie, currentDateSupplier.getDate());
		if (origineleBrief != null)
		{
			projectBrief.setTeHerinnerenBrief(origineleBrief);
		}
		if (origineleBrief != null && origineleBrief.getBriefType() != null)
		{
			projectBrief.setBriefType(origineleBrief.getBriefType());
		}
		projectClient.getBrieven().add(projectBrief);
		hibernateService.saveOrUpdateAll(projectClient, projectBrief);
		return projectBrief;
	}

	private void checkVoorProjectClient(ClientBrief<?, ?, ?> brief, Client client)
	{
		var nu = currentDateSupplier.getDate();
		var projectClient = ProjectUtil.getHuidigeProjectClient(client, nu);
		var actie = ProjectUtil.getProjectBriefActieDefinitie(projectClient, brief.getBriefType());
		if (projectClient != null && actie != null && ProjectUtil.isEinde1eCorrespondentieCheck(nu, projectClient))
		{
			var projectBrief = BriefUtil.maakProjectBrief(projectClient, actie, nu);
			projectBrief.setBrief(brief);
			projectBrief.setBriefType(brief.getBriefType());
			hibernateService.saveOrUpdate(projectBrief);
			brief.setProjectBrief(projectBrief);
			brief.setVervangendeProjectBrief(true);
			hibernateService.saveOrUpdate(brief);
			LOG.info("Brief met type {} vervangen door briefdefinitie {} (clientId: '{}', projectId: '{}')", brief.getBriefType(), actie.getId(), client.getId(),
				projectClient.getProject().getId());
		}
	}

	private <B extends ClientBrief<?, ?, ?>> void vervangDubbeleAangemaakteBrieven(BriefType type, Client client, Class<B> briefClass)
	{

		if (type.getMagNietOpZelfdeDagAfgedruktTypes().isEmpty())
		{
			return;
		}

		BaseJpaRepository<B> repository = getBriefTypeRepository(briefClass);
		var brieven = repository.findAll(heeftOngegeneerdeBrieven(type, client, briefClass));

		for (var brief : brieven)
		{
			if (brief.getProjectBrief() != null)
			{
				brief.getProjectBrief().setVervangen(true);
				hibernateService.saveOrUpdate(brief.getProjectBrief());
			}
			brief.setVervangen(true);
			hibernateService.saveOrUpdate(brief);
		}
	}

	private void checkVoorDubbeleBrieven(BriefType type, CervixHuisarts arts)
	{

		if (!type.getMagNietOpZelfdeDagAfgedruktTypes().isEmpty())
		{
			var brieven = getDubbeleAangemaaktBrieven(type, arts);
			for (var brief : brieven)
			{
				brief.setVervangen(true);
				hibernateService.saveOrUpdate(brief);
			}
		}
	}

	private List<CervixRegioBrief> getDubbeleAangemaaktBrieven(BriefType type, CervixHuisarts arts)
	{
		return cervixRegioBriefRepository.findAll(CervixRegioBriefSpecification.heeftGeenMergedBrieven()
			.and(isNietVervangen())
			.and(heeftBriefTypeIn(type.getMagNietOpZelfdeDagAfgedruktTypes()))
			.and(heeftHuisarts(arts)));
	}

}
