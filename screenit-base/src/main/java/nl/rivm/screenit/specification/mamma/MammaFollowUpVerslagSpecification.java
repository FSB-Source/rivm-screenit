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

import java.util.Date;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.verslag.MammaVerslag_;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpPathologieMedischeObservatie_;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpVerslagContent_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftId;
import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaFollowUpVerslagSpecification
{
	public static Specification<MammaFollowUpVerslag> heeftClientId(Long clientId)
	{
		return heeftId(clientId).with(r ->
		{
			var screeningRondeJoin = join(r, MammaVerslag_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, MammaScreeningRonde_.dossier);
			return join(dossierJoin, MammaDossier_.client);
		});
	}

	public static Specification<MammaFollowUpVerslag> heeftType(VerslagType type)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaVerslag_.type), type);
	}

	public static ExtendedSpecification<MammaFollowUpVerslag> heeftDatumVerwerktVoor(Date datum)
	{
		return (r, q, cb) -> cb.lessThan(r.get(MammaVerslag_.datumVerwerkt), datum);
	}

	public static ExtendedSpecification<MammaFollowUpVerslag> heeftGeenInvoerder()
	{
		return (r, q, cb) -> cb.isNull(r.get(MammaVerslag_.invoerder));
	}

	public static ExtendedSpecification<MammaFollowUpVerslag> heeftPathologieObservatieMetTNummer(String tNummer)
	{
		return (r, q, cb) ->
		{
			var verslagContentJoin = SpecificationUtil.join(r, MammaFollowUpVerslag_.verslagContent);
			var pathologieObservatieJoin = SpecificationUtil.join(verslagContentJoin, MammaFollowUpVerslagContent_.pathologieMedischeObservatie);
			return cb.equal(pathologieObservatieJoin.get(MammaFollowUpPathologieMedischeObservatie_.tnummerLaboratorium), tNummer);
		};
	}
}
