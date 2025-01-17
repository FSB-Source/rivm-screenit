package nl.rivm.screenit.specification.algemeen;

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

import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.UploadDocument_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.endsWithCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.startsWithCaseInsensitive;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class UploadDocumentSpecification
{
	public static Specification<UploadDocument> heeftNaamDieStartMet(String tekst)
	{
		return (r, q, cb) -> startsWithCaseInsensitive(cb, r.get(UploadDocument_.naam), tekst);
	}

	public static Specification<UploadDocument> heeftNaamDieEindigtOp(String tekst)
	{
		return (r, q, cb) -> endsWithCaseInsensitive(cb, r.get(UploadDocument_.naam), tekst);
	}

	public static Specification<UploadDocument> heeftPathDieStartMet(String tekst)
	{
		return (r, q, cb) -> startsWithCaseInsensitive(cb, r.get(UploadDocument_.path), tekst);
	}

	public static Specification<UploadDocument> heeftContentType(String contentType)
	{
		return (r, q, cb) -> cb.equal(r.get(UploadDocument_.contentType), contentType);
	}

	public static Specification<UploadDocument> heeftContentTypeIn(List<String> contentTypes)
	{
		return (r, q, cb) -> r.get(UploadDocument_.contentType).in(contentTypes);
	}
}
