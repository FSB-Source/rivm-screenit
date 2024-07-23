package nl.rivm.screenit.specification.cervix;

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
import java.util.List;

import javax.persistence.criteria.From;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.Dossier_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.Uitnodiging_;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag_;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixHpvBeoordeling_;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht_;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitnodiging_;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje_;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.CervixZas_;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.DateSpecification.truncate;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification.monsterJoin;
import static nl.rivm.screenit.specification.cervix.CervixBriefSpecification.heeftBriefInBrieftypes;
import static nl.rivm.screenit.specification.cervix.CervixBriefSpecification.heeftVervangendeProjectBrief;
import static nl.rivm.screenit.specification.cervix.CervixBriefSpecification.isGegenereerd;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixMonsterSpecification
{
	public static Specification<CervixMonster> heeftBrief()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(CervixMonster_.brief));
	}

	public static Specification<CervixMonster> heeftGeenVerrichtingen()
	{
		return (r, q, cb) -> cb.isEmpty(r.get(CervixMonster_.verrichtingen));
	}

	public static Specification<CervixMonster> heeftGeenOntvangenHuisartsBericht()
	{
		return (r, q, cb) ->
		{
			var status = cb.treat(r, CervixUitstrijkje.class)
				.join(CervixUitstrijkje_.huisartsBericht, JoinType.LEFT)
				.get(CervixHuisartsBericht_.status);

			return cb.or(
				cb.isNull(status),
				cb.notEqual(status, CervixHuisartsBerichtStatus.AANGEMAAKT)
			);
		};
	}

	public static Specification<CervixMonster> isOntvangenUitstrijkje()
	{
		return (r, q, cb) ->
		{
			var status = cb.treat(r, CervixUitstrijkje.class).get(CervixUitstrijkje_.uitstrijkjeStatus);

			return cb.or(
				cb.isNull(status),
				cb.notEqual(status, CervixUitstrijkjeStatus.NIET_ONTVANGEN)
			);
		};
	}

	public static Specification<CervixMonster> isZas()
	{
		return (r, q, cb) -> cb.treat(r, CervixZas.class).get(CervixZas_.zasStatus).isNotNull();
	}

	public static Specification<CervixMonster> geefNietIngestuurdeOudeZAS(CervixDossier dossier)
	{
		return (r, q, cb) ->
			cb.and(
				cb.equal(cb.treat(r, CervixZas.class).get(CervixZas_.zasStatus), CervixZasStatus.VERSTUURD),
				cb.like(r.get(CervixMonster_.monsterId), "Z%"),
				cb.equal(r.join(CervixMonster_.uitnodiging).join(CervixUitnodiging_.screeningRonde).join(CervixScreeningRonde_.dossier), dossier));
	}

	public static Specification<CervixMonster> heeftMonsterId(String monsterId)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixMonster_.monsterId), StringUtils.trimToEmpty(monsterId));
	}

	public static Specification<CervixZas> heeftZasMonsterId(String monsterId)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixMonster_.monsterId), StringUtils.trimToEmpty(monsterId));
	}

	public static Specification<CervixUitstrijkje> heeftUitstrijkjeMonsterId(String monsterId)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixMonster_.monsterId), StringUtils.trimToEmpty(monsterId));
	}

	public static Specification<CervixBoekRegel> filterMonsterId(String monsterId)
	{
		return skipWhenEmpty(monsterId,
			(r, q, cb) -> cb.equal(monsterJoin(r).get(CervixMonster_.monsterId), monsterId));
	}

	public static Specification<CervixUitstrijkje> heeftControleLetters(String controleLetters)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixUitstrijkje_.controleLetters), StringUtils.trimToEmpty(controleLetters));
	}

	public static Specification<CervixUitstrijkje> heeftBsn(String bsn)
	{
		return (r, q, cb) -> cb.equal(
			r.join(CervixMonster_.uitnodiging)
				.join(CervixUitnodiging_.screeningRonde)
				.join(CervixScreeningRonde_.dossier)
				.join(CervixDossier_.client)
				.join(Client_.persoon)
				.get(GbaPersoon_.bsn), bsn);
	}

	public static Specification<CervixMonster> heeftOntvangstRonde(CervixScreeningRonde ontvangstRonde)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixMonster_.ontvangstScreeningRonde), ontvangstRonde);

	}

	public static Specification<CervixMonster> heeftOngeldigeHpvUitslag()
	{
		return (r, q, cb) ->
		{
			var laatsteHpvBeoordelingJoin = join(r, CervixMonster_.laatsteHpvBeoordeling, JoinType.LEFT);
			return cb.equal(laatsteHpvBeoordelingJoin.get(CervixHpvBeoordeling_.hpvUitslag), CervixHpvBeoordelingWaarde.ONGELDIG);
		};
	}

	public static Specification<CervixMonster> heeftPap0CytologieUitslag()
	{
		return (r, q, cb) ->
		{
			var cytologieVerslagJoin = join(cb.treat(r, CervixUitstrijkje.class), CervixUitstrijkje_.cytologieVerslag, JoinType.LEFT);
			return cb.equal(cytologieVerslagJoin.get(CervixCytologieVerslag_.cytologieUitslag), CervixCytologieUitslag.PAP0);
		};
	}

	public static Specification<CervixMonster> heeftVervolgOnderzoekVoorOntvangstDatum()
	{
		return (r, q, cb) ->
		{
			var screeningRondeJoin = join(r, CervixMonster_.ontvangstScreeningRonde);
			return cb.or(
				cb.isNull(screeningRondeJoin.get(CervixScreeningRonde_.inVervolgonderzoekDatum)),
				cb.lessThan(screeningRondeJoin.get(CervixScreeningRonde_.inVervolgonderzoekDatum), r.get(CervixMonster_.ontvangstdatum))
			);
		};
	}

	public static Specification<CervixMonster> isNietHetzelfdeMonster(CervixMonster andersDan)
	{
		return (r, q, cb) -> cb.notEqual(r.get(SingleTableHibernateObject_.id), andersDan.getId());
	}

	public static <T extends CervixMonster> Specification<CervixMonster> isMonsterType(Class<T> type)
	{
		return (r, q, cb) -> cb.equal(r.type(), type);
	}

	public static Specification<CervixMonster> heeftOntvangstDatumOpOfVoor(LocalDate minimaleSignaleringsDatum)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(CervixMonster_.ontvangstdatum), DateUtil.toUtilDate(minimaleSignaleringsDatum));
	}

	public static Specification<CervixMonster> heeftOntvangstDatumNa(LocalDate signalerenVanaf)
	{
		return (r, q, cb) -> cb.greaterThan(r.get(CervixMonster_.ontvangstdatum), DateUtil.toUtilDate(signalerenVanaf));
	}

	public static Specification<CervixMonster> heeftDossierMetDatumLaatstGecontroleerdeSignaleringNa(LocalDate signalerenVanaf)
	{
		return (r, q, cb) ->
		{
			var dossierJoin = dossierJoin(r);
			return cb.greaterThan(dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering), DateUtil.toUtilDate(signalerenVanaf));
		};
	}

	public static Specification<CervixMonster> heeftOntvangstDatumNaLaatstGecontroleerdeSignalering()
	{
		return (r, q, cb) ->
		{
			var dossierJoin = dossierJoin(r);
			return cb.greaterThan(
				truncate("day", r.get(CervixMonster_.ontvangstdatum), cb),
				truncate("day", dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering), cb));
		};
	}

	public static Specification<CervixMonster> valtDatumLaatstGecontroleerdeSignaleringOpOfIsVoor(LocalDate signalerenVanaf)
	{
		return (r, q, cb) ->
		{
			var dossierJoin = dossierJoin(r);
			return cb.lessThanOrEqualTo(truncate("day", dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering), cb),
				DateUtil.toUtilDate(signalerenVanaf));
		};
	}

	public static Specification<CervixMonster> heeftGeenGecontroleerdeSignaleringDatum()
	{
		return (r, q, cb) ->
		{
			var dossierJoin = dossierJoin(r);
			return cb.isNull(dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering));
		};
	}

	public static Specification<CervixMonster> heeftActieveClient()
	{
		return ClientSpecification.heeftActieveClientPredicate().toSpecification(r ->
		{
			var dossierJoin = dossierJoin(r);
			return join(dossierJoin, CervixDossier_.client);
		});
	}

	public static Specification<CervixMonster> heeftDossier(CervixDossier dossier)
	{
		return (r, q, cb) ->
		{
			var rondeJoin = join(r, CervixMonster_.ontvangstScreeningRonde);
			return cb.equal(rondeJoin.get(CervixScreeningRonde_.dossier), dossier);
		};
	}

	public static Specification<CervixMonster> heeftGeenSignalering(LocalDate signalerenVanaf)
	{
		return (r, q, cb) ->
			cb.or(
				cb.and(heeftDossierMetDatumLaatstGecontroleerdeSignaleringNa(signalerenVanaf).toPredicate(r, q, cb),
					heeftOntvangstDatumNaLaatstGecontroleerdeSignalering().toPredicate(r, q, cb)),
				cb.and(
					cb.or(
						valtDatumLaatstGecontroleerdeSignaleringOpOfIsVoor(signalerenVanaf).toPredicate(r, q, cb),
						heeftGeenGecontroleerdeSignaleringDatum().toPredicate(r, q, cb))
					, heeftOntvangstDatumNa(signalerenVanaf).toPredicate(r, q, cb))
			);
	}

	public static Specification<CervixMonster> heeftMonsterMetMissendeUitslag()
	{
		return (r, q, cb) ->
			cb.or(
				cb.and(
					cb.or(cb.notEqual(cb.treat(r, CervixUitstrijkje.class).get(CervixUitstrijkje_.uitstrijkjeStatus), CervixUitstrijkjeStatus.NIET_ONTVANGEN),
						cb.notEqual(cb.treat(r, CervixZas.class).get(CervixZas_.zasStatus), CervixZasStatus.VERSTUURD)),
					heeftGeenMonsterMetUitslagBriefSubquery().toPredicate(r, q, cb)
				)
				, cb.and(
					cb.equal(cb.treat(r, CervixZas.class).get(CervixZas_.zasStatus), CervixZasStatus.NIET_ANALYSEERBAAR),
					cb.not(heeftZasInZelfdeRondeSubquery().toPredicate(r, q, cb)),
					cb.not(isMonsterDatOntvangenIsNaResultaat().toPredicate(r, q, cb))
				)
			);

	}

	private static Specification<CervixMonster> heeftGeenMonsterMetUitslagBriefSubquery()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(CervixBrief.class);
			var projectBriefJoin = subRoot.join(ClientBrief_.projectBrief, JoinType.LEFT);
			subquery.select(subRoot.get(TablePerClassHibernateObject_.id)).where(cb.and(
				cb.equal(subRoot, r.get(CervixMonster_.brief)), 
				cb.or(
					cb.and(
						heeftVervangendeProjectBrief(false).withPath(cb, subRoot),
						heeftBriefInBrieftypes(BriefType.CERVIX_UITSLAG_BRIEVEN).withPath(cb, subRoot),
						isGegenereerd(true).withPath(cb, subRoot)
					),
					cb.and(
						heeftVervangendeProjectBrief(true).withPath(cb, subRoot),
						ProjectBriefSpecification.heeftBriefInBrieftypes(BriefType.CERVIX_UITSLAG_BRIEVEN).withPath(cb, projectBriefJoin),
						ProjectBriefSpecification.isGegenereerd(true).withPath(cb, projectBriefJoin)
					))));
			return cb.not(cb.exists(subquery));
		};
	}

	private static Specification<CervixMonster> heeftZasInZelfdeRondeSubquery()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(CervixUitnodiging.class);
			var subqueryBriefJoin = subRoot.join(CervixUitnodiging_.brief);

			var mainQueryScreeningRondeJoin = join(r, CervixMonster_.ontvangstScreeningRonde);
			subquery.select(subRoot.get(TablePerClassHibernateObject_.id)).where(
				cb.and(
					cb.equal(subRoot.get(CervixUitnodiging_.monsterType), CervixMonsterType.ZAS),
					cb.equal(subRoot.get(CervixUitnodiging_.screeningRonde), mainQueryScreeningRondeJoin),
					cb.greaterThanOrEqualTo(truncate("day", subRoot.get(Uitnodiging_.creatieDatum), cb),
						truncate("day", r.get(CervixMonster_.statusDatum), cb)),
					cb.equal(subqueryBriefJoin.get(Brief_.gegenereerd), true)
				)
			);
			return cb.exists(subquery);
		};
	}

	private static Specification<CervixMonster> isMonsterDatOntvangenIsNaResultaat()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(CervixUitnodiging.class);
			var monsterJoin = subRoot.join(CervixUitnodiging_.monster);
			var briefJoin = monsterJoin.join(CervixMonster_.brief);

			subquery.select(subRoot.get(TablePerClassHibernateObject_.id)).where(
				cb.and(
					cb.equal(monsterJoin, r),
					cb.equal(subRoot.get(CervixUitnodiging_.monsterType), CervixMonsterType.ZAS),
					briefJoin.get(Brief_.briefType).in(List.of(BriefType.CERVIX_MONSTER_NA_HPV_NEGATIEF, BriefType.CERVIX_ZAS_NA_HPV_POSITIEF))
				)
			);

			return cb.exists(subquery);
		};
	}

	private static From<CervixScreeningRonde, CervixDossier> dossierJoin(Root<CervixMonster> root)
	{
		var rondeJoin = join(root, CervixMonster_.ontvangstScreeningRonde);
		return join(rondeJoin, CervixScreeningRonde_.dossier);
	}

}
