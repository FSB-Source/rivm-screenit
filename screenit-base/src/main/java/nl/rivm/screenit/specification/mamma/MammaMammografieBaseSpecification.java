package nl.rivm.screenit.specification.mamma;

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

import java.time.LocalDateTime;
import java.util.List;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaMammografie_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaDenseWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaMammografieBaseSpecification
{
	public static ExtendedSpecification<MammaMammografie> heeftDensiteit(MammaDenseWaarde densiteit)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaMammografie_.densiteit), densiteit);
	}

	public static ExtendedSpecification<MammaMammografie> heeftDensiteit()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(MammaMammografie_.densiteit));
	}

	public static Specification<MammaMammografie> heeftUitnodigingsNummer(long uitnodigingsNummer)
	{
		return MammaScreeningRondeSpecification.heeftUitnogigingsNummer(uitnodigingsNummer).with(r -> screeningRondeJoin(r));
	}

	public static ExtendedSpecification<MammaMammografie> heeftIlmStatusDatumVoor(LocalDateTime peilMoment)
	{
		return (r, q, cb) -> cb.lessThan(r.get(MammaMammografie_.ilmStatusDatum), DateUtil.toUtilDate(peilMoment));
	}

	public static ExtendedSpecification<MammaMammografie> heeftIlmStatus(MammaMammografieIlmStatus ilmStatus)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaMammografie_.ilmStatus), ilmStatus);
	}

	public static ExtendedSpecification<MammaMammografie> heeftIlmStatusIn(List<MammaMammografieIlmStatus> ilmStatussen)
	{
		return (r, q, cb) -> r.get(MammaMammografie_.ilmStatus).in(ilmStatussen);
	}

	private static Join<?, MammaScreeningRonde> screeningRondeJoin(From<?, ? extends MammaMammografie> root)
	{
		var onderzoekJoin = SpecificationUtil.join(root, MammaMammografie_.onderzoek);
		var afspraakJoin = SpecificationUtil.join(onderzoekJoin, MammaOnderzoek_.afspraak);
		var uitnodigingJoin = SpecificationUtil.join(afspraakJoin, MammaAfspraak_.uitnodiging);
		return SpecificationUtil.join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
	}
}
