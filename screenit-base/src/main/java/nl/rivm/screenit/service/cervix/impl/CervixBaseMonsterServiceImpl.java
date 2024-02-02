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

import java.util.Optional;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.cervix.CervixHpvAnalyseresultaten;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultValue;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.repository.cervix.CervixBaseMonsterRepository;
import nl.rivm.screenit.repository.cervix.CervixUitstrijkjeRepository;
import nl.rivm.screenit.repository.cervix.CervixZasRepository;
import nl.rivm.screenit.service.cervix.CervixBaseMonsterService;
import nl.rivm.screenit.specification.cervix.CervixMonsterSpecification;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.envers.AuditReader;
import org.hibernate.envers.AuditReaderFactory;
import org.hibernate.envers.query.AuditEntity;
import org.hibernate.envers.query.AuditQuery;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CervixBaseMonsterServiceImpl implements CervixBaseMonsterService
{
	private final CervixUitstrijkjeRepository uitstrijkjeRepository;

	private final CervixZasRepository zasRepository;

	private final CervixBaseMonsterRepository monsterRepository;

	private final HibernateService hibernateService;

	@Override
	public Optional<CervixUitstrijkje> getUitstrijkjeByClientBsnAndMonsterId(String bsn, String monsterId)
	{
		return uitstrijkjeRepository.findOne(CervixMonsterSpecification.heeftUitstrijkjeMonsterId(monsterId).and(CervixMonsterSpecification.heeftBsn(bsn)));
	}

	@Override
	public Optional<CervixUitstrijkje> getUitstrijkjeByClientBsnAndControleLetters(String bsn, String controleLetters)
	{
		return uitstrijkjeRepository.findOne(CervixMonsterSpecification.heeftControleLetters(controleLetters).and(CervixMonsterSpecification.heeftBsn(bsn)));
	}

	@Override
	public Optional<CervixUitstrijkje> getUitstrijkje(String monsterId)
	{
		return uitstrijkjeRepository.findOne(CervixMonsterSpecification.heeftUitstrijkjeMonsterId(monsterId));
	}

	@Override
	public Optional<CervixZas> getZas(String monsterId)
	{
		return zasRepository.findOne(CervixMonsterSpecification.heeftZasMonsterId(monsterId));
	}

	@Override
	public Optional<CervixMonster> getMonster(String monsterId)
	{
		return monsterRepository.findOne(CervixMonsterSpecification.heeftMonsterId(monsterId));
	}

	@Override
	public boolean monsterHeeftHpvBeoordelingMetGenotypeOther(CervixMonster monsterHpvUitslag)
	{
		CervixHpvAnalyseresultaten analyseresultaten = monsterHpvUitslag.getLaatsteHpvBeoordeling().getAnalyseresultaten();

		return analyseresultaten != null && !CervixHpvResultValue.POS_HPV16.equals(analyseresultaten.getHpv16()) && !CervixHpvResultValue.POS_HPV18.equals(
			analyseresultaten.getHpv18()) && CervixHpvResultValue.POS_OTHER_HR_HPV.equals(analyseresultaten.getHpvohr());
	}

	@Override
	public boolean magInstellingMonsterInzien(Instelling instelling, CervixMonster monster)
	{
		boolean isMonsterNietOntvangen;
		if (CervixMonsterUtil.isUitstrijkje(monster))
		{
			isMonsterNietOntvangen = CervixUitstrijkjeStatus.NIET_ONTVANGEN == CervixMonsterUtil.getUitstrijkje(monster).getUitstrijkjeStatus();
		}
		else
		{
			isMonsterNietOntvangen = CervixZasStatus.VERSTUURD == CervixMonsterUtil.getZAS(monster).getZasStatus();
		}
		return isMonsterNietOntvangen || instelling.getOrganisatieType() != OrganisatieType.BMHK_LABORATORIUM || instelling.equals(monster.getLaboratorium());
	}

	@Override
	public boolean isVerwijderdMonster(String monsterId)
	{
		var auditReader = AuditReaderFactory.get(hibernateService.getHibernateSession());
		var auditQuery = auditReader.createQuery().forRevisionsOfEntity(CervixMonster.class, false, true)
			.add(AuditEntity.property("monsterId").eq(monsterId))
			.addProjection(AuditEntity.id().count());

		return ((Long) auditQuery.getSingleResult()) > 0;
	}
}
