package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.project;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.StringIsIntegerValidator;
import nl.rivm.screenit.model.ProjectParameter;
import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.markup.form.validation.UniqueFieldValidator;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.ValidationError;
import org.apache.wicket.validation.validator.RangeValidator;
import org.apache.wicket.validation.validator.StringValidator;

public class EditProjectParametersPanel extends GenericPanel<Project>
{
	@SpringBean
	private HibernateService hibernateService;

	private List<Bevolkingsonderzoek> bevolkingsonderzoeken;

	public EditProjectParametersPanel(String id, IModel<Project> model, List<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		super(id, model);
		this.bevolkingsonderzoeken = bevolkingsonderzoeken;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		Form<Void> form = new Form<>("form");
		add(form);

		List<ProjectParameter> parameters = getModelObject().getParameters();
		RepeatingView parametersView = new RepeatingView("parameters");
		List<ProjectParameterKey> parameterKeysVanParameters = parameters.stream().map(ProjectParameter::getKey).collect(Collectors.toList());
		for (ProjectParameterKey parameterKey : ProjectParameterKey.values())
		{
			int i = parameterKeysVanParameters.indexOf(parameterKey);
			if (i >= 0)
			{
				addParameterKeyRowToParametersView(parametersView, parameterKey, i);
			}
		}
		form.add(parametersView);
	}

	private void addParameterKeyRowToParametersView(RepeatingView parametersView, ProjectParameterKey parameterKey, int i)
	{
		IModel<ProjectParameter> parameterModel = new CompoundPropertyModel<>(new PropertyModel<>(getModel(), "parameters[" + i + "]"));
		final WebMarkupContainer parameterRow = new WebMarkupContainer(parametersView.newChildId(), parameterModel);
		ProjectParameter parameter = parameterModel.getObject();

		parameterRow.add(new EnumLabel<ProjectParameterKey>("key"));

		FormComponent<String> valueField = ComponentHelper.addTextField(this, "value", false, 9, false);
		valideerValueField(parameterKey, parameter, valueField);
		parameterRow.add(valueField);

		ScreenitDropdown<ColonOnderzoeksVariant> onderzoeksvariantDropDown = maakOnderzoeksvariantDropdown(parameterModel);

		if (!parameterKey.getValueType().equals(ColonOnderzoeksVariant.class))
		{
			onderzoeksvariantDropDown.setVisible(false);
		}
		else
		{
			valueField.setVisible(false);
			if (parameter.getValue() == null)
			{
				parameter.setValue(ColonOnderzoeksVariant.STANDAARD.name());
			}
		}
		parameterRow.add(onderzoeksvariantDropDown);

		parameterRow.add(new Label("unit", getString(EnumStringUtil.getPropertyString(parameterKey) + ".unit")));
		parameterRow.setVisible(bevolkingsonderzoeken.contains(parameterKey.getBevolkingsonderzoek())
			&& parameterKey.getProjectType().equals(getModelObject().getType()));
		parametersView.add(parameterRow);
	}

	private ScreenitDropdown<ColonOnderzoeksVariant> maakOnderzoeksvariantDropdown(IModel<ProjectParameter> parameterModel)
	{
		List<ColonOnderzoeksVariant> onderzoeksvarianten = Arrays.asList(ColonOnderzoeksVariant.STANDAARD, ColonOnderzoeksVariant.VERGELIJKEND);
		ScreenitDropdown<ColonOnderzoeksVariant> onderzoeksvariantDropDown = ComponentHelper.addDropDownChoice(this, "enumValue", true, onderzoeksvarianten, false);
		onderzoeksvariantDropDown.setModel(new IModel<>()
		{
			@Override
			public ColonOnderzoeksVariant getObject()
			{
				ProjectParameter parameter = parameterModel.getObject();
				String value = parameter.getValue();
				return StringUtils.isNotBlank(value) ? ColonOnderzoeksVariant.valueOf(value) : null;
			}

			@Override
			public void setObject(ColonOnderzoeksVariant object)
			{
				ProjectParameter parameter = parameterModel.getObject();
				parameter.setValue(object.name());
			}
		});
		onderzoeksvariantDropDown.setNullValid(false);

		return onderzoeksvariantDropDown;
	}

	private void valideerValueField(ProjectParameterKey parameterKey, ProjectParameter parameter, FormComponent<String> valueField) {
		if (parameterKey.isUniek())
		{
			addUniqueFieldValidator(parameter, parameterKey, valueField);
		}

		Class<?> valueType = parameterKey.getValueType();
		if (valueType.equals(Integer.class))
		{
			addIntegerValidators(parameterKey, valueField);
		}
		else if (valueType.equals(BigDecimal.class))
		{
			addBigDecimalValidator(parameterKey, valueField);
		}
		else if (valueType.equals(String.class))
		{
			valueField.add(StringValidator.maximumLength(255));
		}
	}

	private void addUniqueFieldValidator(ProjectParameter parameter, ProjectParameterKey parameterKey, FormComponent<String> valueField)
	{
		Map<String, Object> restrictions = new HashMap<>();
		restrictions.put("key", parameterKey);

		valueField.add(new UniqueFieldValidator<>(ProjectParameter.class, parameter.getId(), "value", hibernateService, restrictions)
		{
			@Override
			public void validate(IValidatable<String> validatable)
			{
				restrictions.put("value", validatable.getValue());

				if (hibernateService.existsOther(ProjectParameter.class, getObjectId(), restrictions, false))
				{
					ValidationError error = new ValidationError();

					error.addKey("UniqueFieldValidator");
					error.getVariables().put("field", getString(EnumStringUtil.getPropertyString(parameterKey)));
					validatable.error(error);
				}
			}
		});
	}

	private void addIntegerValidators(ProjectParameterKey parameterKey, FormComponent<String> valueField)
	{
		valueField.add(new StringIsIntegerValidator());
		valueField.add(new RangeValidator<>(parameterKey.getMinValue(), parameterKey.getMaxValue())
		{
			@Override
			protected Integer getValue(IValidatable<Integer> validatable)
			{
				Object value = validatable.getValue();
				if (value != null)
				{
					return Integer.valueOf(value.toString());
				}
				return null;
			}
		});
	}

	private void addBigDecimalValidator(ProjectParameterKey parameterKey, FormComponent<String> valueField)
	{
		valueField.add(new RangeValidator<>(BigDecimal.valueOf(parameterKey.getMinValue()), BigDecimal.valueOf(parameterKey.getMaxValue()))
		{

			@Override
			public void validate(IValidatable<BigDecimal> validatable)
			{
				Object value = validatable.getValue();
				if (value != null)
				{
					try
					{
						BigDecimalUtil.stringToBigDecimal(value.toString(), Constants.LOCALE_NL);
					}
					catch (Exception e)
					{
						ValidationError error = new ValidationError(this, "bigdecimal");
						validatable.error(error);
					}
				}
				if (validatable.isValid())
				{
					super.validate(validatable);
				}
			}

			@Override
			protected BigDecimal getValue(IValidatable<BigDecimal> validatable)
			{
				Object value = validatable.getValue();
				if (value != null)
				{
					return BigDecimalUtil.stringToBigDecimal(value.toString(), Constants.LOCALE_NL);
				}
				return null;
			}
		});
	}
}
