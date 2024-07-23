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

import java.time.LocalDate;
import java.util.Optional;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultValue;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.repository.cervix.CervixBaseMonsterRepository;
import nl.rivm.screenit.repository.cervix.CervixUitstrijkjeRepository;
import nl.rivm.screenit.repository.cervix.CervixZasRepository;
import nl.rivm.screenit.service.cervix.CervixBaseMonsterService;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.envers.AuditReaderFactory;
import org.hibernate.envers.query.AuditEntity;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftBsn;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftControleLetters;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftDossier;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftGeenSignalering;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftMonsterId;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftMonsterMetMissendeUitslag;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftOntvangstDatumOpOfVoor;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftUitstrijkjeMonsterId;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftZasMonsterId;

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
		return uitstrijkjeRepository.findOne(heeftUitstrijkjeMonsterId(monsterId).and(heeftBsn(bsn)));
	}

	@Override
	public Optional<CervixUitstrijkje> getUitstrijkjeByClientBsnAndControleLetters(String bsn, String controleLetters)
	{
		return uitstrijkjeRepository.findOne(heeftControleLetters(controleLetters).and(heeftBsn(bsn)));
	}

	@Override
	public Optional<CervixUitstrijkje> getUitstrijkje(String monsterId)
	{
		return uitstrijkjeRepository.findOne(heeftUitstrijkjeMonsterId(monsterId));
	}

	@Override
	public Optional<CervixZas> getZas(String monsterId)
	{
		return zasRepository.findOne(heeftZasMonsterId(monsterId));
	}

	@Override
	public Optional<CervixMonster> getMonster(String monsterId)
	{
		return monsterRepository.findOne(heeftMonsterId(monsterId));
	}

	@Override
	public boolean monsterHeeftHpvBeoordelingMetGenotypeOther(CervixMonster monsterHpvUitslag)
	{
		var analyseresultaten = monsterHpvUitslag.getLaatsteHpvBeoordeling().getAnalyseresultaten();

		return analyseresultaten != null && CervixHpvResultValue.NEG_HPV16.equals(analyseresultaten.getHpv16()) && CervixHpvResultValue.NEG_HPV18.equals(
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

	@Override
	public Optional<CervixMonster> getLaatsteMonsterMetMissendeUitslagVanDossier(CervixDossier dossier, LocalDate signalerenVanaf, LocalDate minimaleSignaleringsDatum)
	{
		return monsterRepository.findFirst(maakMonsterMetMissendeUitslagSpecification(signalerenVanaf, minimaleSignaleringsDatum).and(heeftDossier(dossier)),
			Sort.by(Sort.Direction.ASC, CervixMonster_.ONTVANGSTDATUM));
	}

	@Override
	public Specification<CervixMonster> maakMonsterMetMissendeUitslagSpecification(LocalDate signalerenVanaf, LocalDate minimaleSignaleringsDatum)
	{
		return heeftOntvangstDatumOpOfVoor(minimaleSignaleringsDatum)
			.and(heeftGeenSignalering(signalerenVanaf))
			.and(heeftMonsterMetMissendeUitslag())
			.and(heeftActieveClient());
	}
}
