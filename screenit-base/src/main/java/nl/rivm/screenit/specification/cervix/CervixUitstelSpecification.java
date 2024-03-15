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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.ScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.CervixUitstel_;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixUitstelSpecification
{
	public static Specification<CervixUitstel> heeftGeenGeannuleerdDatum()
	{
		return (r, q, cb) -> cb.isNull(r.get(CervixUitstel_.geannuleerdDatum));
	}

	public static Specification<CervixUitstel> heeftUitstellenTotDatumEerderDan(LocalDate datum)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(CervixUitstel_.uitstellenTotDatum), DateUtil.toUtilDate(datum));
	}

	public static Specification<CervixUitstel> heeftLopendeRonde()
	{
		return (r, q, cb) ->
		{
			var ronde = join(r, CervixUitstel_.screeningRonde);
			return cb.equal(ronde.get(ScreeningRonde_.status), ScreeningRondeStatus.LOPEND);
		};
	}

	public static Specification<CervixUitstel> heeftGeenVertrokkenPersoonUitNederlandDatum()
	{
		return PersoonSpecification.heeftGeenVertrokkenUitNederlandDatumPredicate()
			.toSpecification(r ->
			{
				var ronde = join(r, CervixUitstel_.screeningRonde);
				var dossier = join(ronde, CervixScreeningRonde_.dossier);
				var client = join(dossier, CervixDossier_.client);
				return join(client, Client_.persoon);
			});
	}

	public static Specification<CervixUitstel> heeftGeenPersoonMetOverledenDatum()
	{
		return PersoonSpecification.heeftGeenOverledenDatumPredicate()
			.toSpecification(r ->
			{
				var ronde = join(r, CervixUitstel_.screeningRonde);
				var dossier = join(ronde, CervixScreeningRonde_.dossier);
				var client = join(dossier, CervixDossier_.client);
				return join(client, Client_.persoon);
			});
	}

	public static Specification<CervixUitstel> heeftClientMetIndicatieAanwezig()
	{
		return ClientSpecification.heeftIndicatie()
			.toSpecification(r ->
			{
				var ronde = join(r, CervixUitstel_.screeningRonde);
				var dossier = join(ronde, CervixScreeningRonde_.dossier);
				return join(dossier, CervixDossier_.client);
			});
	}

}
