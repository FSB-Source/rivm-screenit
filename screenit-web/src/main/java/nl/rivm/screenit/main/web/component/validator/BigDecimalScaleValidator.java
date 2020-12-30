package nl.rivm.screenit.main.web.component.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.math.BigDecimal;

import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidator;
import org.apache.wicket.validation.ValidationError;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BigDecimalScaleValidator implements IValidator<BigDecimal>
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(BigDecimalScaleValidator.class);

	private int scaleMax;

	public BigDecimalScaleValidator()
	{
		this.scaleMax = 2;
	}

	public BigDecimalScaleValidator(int scaleMax)
	{
		this.scaleMax = scaleMax;
	}

	@Override
	public void validate(IValidatable<BigDecimal> iValidatable)
	{

		final BigDecimal value = iValidatable.getValue();
		if (value.scale() > scaleMax)
		{
			error(iValidatable, "too.much.scale");
		}
	}

	private void error(IValidatable<BigDecimal> validatable, String errorKey)
	{
		ValidationError error = new ValidationError();
		String errorCode = getClass().getSimpleName() + "." + errorKey;
		LOG.info(errorCode);
		error.addKey(errorCode);
		validatable.error(error);
	}
}
