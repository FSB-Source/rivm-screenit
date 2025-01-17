package nl.rivm.screenit.specification.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Dossier_;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie_;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.algemeen.DossierSpecification;
import nl.rivm.screenit.util.DateUtil;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixDossierSpecification
{
	public static ExtendedSpecification<CervixDossier> heeftVolgendeRondeVoorOfOp(LocalDate peilDatum)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(CervixDossier_.volgendeRondeVanaf), DateUtil.toUtilDate(peilDatum));
	}

	public static ExtendedSpecification<CervixDossier> heeftGeenVolgendeRondeVanaf()
	{
		return (r, q, cb) -> cb.isNull(r.get(CervixDossier_.volgendeRondeVanaf));
	}

	public static ExtendedSpecification<CervixDossier> magNietDeelnemen(LocalDate peilDatum)
	{
		return DossierSpecification.<CervixDossier> heeftDeelnamemodus(Deelnamemodus.SELECTIEBLOKKADE).and(heeftVolgendeRondeVoorOfOp(peilDatum));
	}

	public static ExtendedSpecification<CervixDossier> heeftGeenScreeningRondeInCISHistorie()
	{
		return (r, q, cb) ->
		{
			var cisHistorieJoin = join(r, CervixDossier_.cisHistorie, JoinType.LEFT);
			return cb.equal(r.get(CervixDossier_.laatsteScreeningRonde), cisHistorieJoin.get(CervixCISHistorie_.screeningRonde));
		};
	}

	public static ExtendedSpecification<CervixDossier> heeftGeenLaatsteScreeningronde()
	{
		return (r, q, cb) -> cb.isNull(r.get(CervixDossier_.laatsteScreeningRonde));
	}

	public static ExtendedSpecification<CervixDossier> heeftGeenVooraankondigingsBrief()
	{
		return (r, q, cb) -> cb.isNull(r.get(CervixDossier_.vooraankondigingsBrief));
	}

	public static ExtendedSpecification<CervixDossier> wachtOpStartProject(Boolean wachtOpStartProject)
	{
		return (r, q, cb) -> cb.equal(r.get(Dossier_.wachtOpStartProject), wachtOpStartProject);
	}
}
