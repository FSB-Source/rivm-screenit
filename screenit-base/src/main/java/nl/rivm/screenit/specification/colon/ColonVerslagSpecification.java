package nl.rivm.screenit.specification.colon;

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
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColonVerslag_;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.MdlVerslag_;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.colon.PaVerslag_;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlColoscopieMedischeObservatie_;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlTnummerPathologieVerslag_;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent_;
import nl.rivm.screenit.model.colon.verslag.pa.PaPathologieMedischeObservatie_;
import nl.rivm.screenit.model.colon.verslag.pa.PaVerslagContent_;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValue_;
import nl.rivm.screenit.specification.ExtendedSpecification;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftId;
import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonVerslagSpecification
{
	public static Specification<MdlVerslag> heeftScreeningRondeInMdlVerslag(ColonScreeningRonde screeningRonde)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonVerslag_.screeningRonde), screeningRonde);
	}

	public static Specification<MdlVerslag> heeftTNummerInPaVerslag(String tNummerLaboratorium)
	{
		return (r, q, cb) ->
		{
			var verslagContentJoin = join(r, MdlVerslag_.verslagContent);
			var coloscopieMedischeObservatieJoin = join(verslagContentJoin, MdlVerslagContent_.coloscopieMedischeObservatie);
			var tNummerPathologieVerslagJoin = join(coloscopieMedischeObservatieJoin, MdlColoscopieMedischeObservatie_.tnummerPathologieVerslag);
			return cb.equal(tNummerPathologieVerslagJoin.get(MdlTnummerPathologieVerslag_.tnummerPathologieVerslag),
				tNummerLaboratorium);
		};
	}

	public static Specification<PaVerslag> heeftScreeningRondeInPaVerslag(ColonScreeningRonde screeningRonde)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonVerslag_.screeningRonde), screeningRonde);
	}

	public static Specification<PaVerslag> heeftTNummerIn(List<String> tNummers)
	{
		return (r, q, cb) ->
		{
			var verslagContentJoin = join(r, PaVerslag_.verslagContent);
			var pathologischeMedischeObservatieJoin = join(verslagContentJoin, PaVerslagContent_.pathologieMedischeObservatie);

			return pathologischeMedischeObservatieJoin.get(PaPathologieMedischeObservatie_.tnummerLaboratorium).in(tNummers);
		};
	}

	public static Specification<DSValue> heeftCode(String code, boolean ignoreCase)
	{
		return (r, q, cb) ->
		{
			if (ignoreCase)
			{
				return cb.equal(cb.lower(r.get(DSValue_.code)), StringUtils.lowerCase(code));
			}
			return cb.equal(r.get(DSValue_.code), code);
		};
	}

	public static Specification<DSValue> heeftCodeSystem(String codeSystem)
	{
		return (r, q, cb) -> cb.equal(r.get(DSValue_.codeSystem), codeSystem);
	}

	public static Specification<DSValue> heeftValueSetName(String valueSetName)
	{
		return (r, q, cb) -> cb.equal(r.get(DSValue_.valueSetName), valueSetName);
	}

	public static Specification<MdlVerslag> heeftColonDossier(ColonDossier dossier)
	{
		return (r, q, cb) ->
		{
			var screeningRondeJoin = join(r, ColonVerslag_.screeningRonde);
			return cb.equal(screeningRondeJoin.get(ColonScreeningRonde_.dossier), dossier);
		};
	}

	public static Specification<MdlVerslag> heeftVerslagStatus(VerslagStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonVerslag_.status), status);
	}

	public static ExtendedSpecification<ColonVerslag> heeftColonVerslagStatus(VerslagStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonVerslag_.status), status);
	}

	public static Specification<MdlVerslag> heeftDatumOnderzoek(Date datumOnderzoek)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonVerslag_.datumOnderzoek), datumOnderzoek);
	}

	public static Specification<MdlVerslag> heeftClientIdInMdlVerslag(Long clientId)
	{
		return heeftId(clientId).with(r ->
		{
			var screeningRondeJoin = join(r, ColonVerslag_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, ColonScreeningRonde_.dossier);
			return join(dossierJoin, ColonDossier_.client);
		});
	}

	public static Specification<ColonVerslag> heeftClientId(Long clientId)
	{
		return heeftId(clientId).with(r ->
		{
			var screeningRondeJoin = join(r, ColonVerslag_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, ColonScreeningRonde_.dossier);
			return join(dossierJoin, ColonDossier_.client);
		});
	}

	public static Specification<MdlVerslag> heeftTypeInMdlVerslag(VerslagType type)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonVerslag_.type), type);
	}

	public static Specification<PaVerslag> heeftClientIdInPaVerslag(Long clientId)
	{
		return heeftId(clientId).with(r ->
		{
			var screeningRondeJoin = join(r, ColonVerslag_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, ColonScreeningRonde_.dossier);
			return join(dossierJoin, ColonDossier_.client);
		});
	}

	public static Specification<PaVerslag> heeftTypeInPaVerslag(VerslagType type)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonVerslag_.type), type);
	}

	public static Specification<ColonVerslag> heeftType(VerslagType type)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonVerslag_.type), type);
	}
}
