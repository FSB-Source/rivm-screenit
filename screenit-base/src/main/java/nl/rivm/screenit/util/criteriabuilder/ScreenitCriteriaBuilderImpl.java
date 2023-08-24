package nl.rivm.screenit.util.criteriabuilder;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.Serializable;
import java.util.Date;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Predicate;

import org.hibernate.query.criteria.internal.CriteriaBuilderImpl;
import org.hibernate.query.criteria.internal.Renderable;
import org.hibernate.query.criteria.internal.compile.RenderingContext;
import org.hibernate.query.criteria.internal.predicate.ComparisonPredicate;

public class ScreenitCriteriaBuilderImpl extends CriteriaBuilderImpl
{

	class DateCompWithIntervalPredicate extends ComparisonPredicate implements Serializable
	{
		private final String interval;

		public DateCompWithIntervalPredicate(
			CriteriaBuilderImpl criteriaBuilder,
			ComparisonOperator comparisonOperator,
			Expression<Date> leftExpression,
			Expression<Date> rightExpression,
			String interval)
		{
			super(criteriaBuilder, comparisonOperator, leftExpression, rightExpression);

			this.interval = interval;
		}

		@Override
		public String render(boolean isNegated, RenderingContext renderingContext)
		{
			return ((Renderable) getLeftHandOperand()).render(renderingContext)
				+ getComparisonOperator(isNegated).rendered()
				+ "(" + ((Renderable) getRightHandOperand()).render(renderingContext) + " " + interval + ")";
		}

	}

	public ScreenitCriteriaBuilderImpl(CriteriaBuilder criteriaBuilder)
	{
		super(((CriteriaBuilderImpl) criteriaBuilder).getEntityManagerFactory());
	}

	public Predicate greaterThanOrEqualTo(Expression<Date> left, Expression<Date> right, String rightInterval)
	{
		return new DateCompWithIntervalPredicate(this, ComparisonPredicate.ComparisonOperator.GREATER_THAN_OR_EQUAL, left, right, rightInterval);
	}

	public Predicate lessThanOrEqualTo(Expression<Date> left, Expression<Date> right, String rightInterval)
	{
		return new DateCompWithIntervalPredicate(this, ComparisonPredicate.ComparisonOperator.LESS_THAN_OR_EQUAL, left, right, rightInterval);
	}

}
