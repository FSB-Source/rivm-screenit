package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieParameter;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.StringIsNumberValidator;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.ValidationError;
import org.apache.wicket.validation.validator.RangeValidator;
import org.apache.wicket.validation.validator.StringValidator;

public abstract class EditOrganisatieParametersHorizontalPanel extends GenericPanel<List<OrganisatieParameterKey>>
{

	@SpringBean
	private InstellingService instellingService;

	private IModel<List<OrganisatieParameter>> allParametersModel = ModelUtil.listModel(new ArrayList<>(), false);

	protected EditOrganisatieParametersHorizontalPanel(String id, List<OrganisatieParameterKey> parameterKeys, boolean valueFieldEnabled)
	{
		super(id);

		Form<Void> form = new Form<>("form");
		add(form);

		form.add(new ListView<>("organisatieTypes", parameterKeys.stream().map(OrganisatieParameterKey::getOrganisatieType).distinct().collect(Collectors.toList()))
		{

			@Override
			protected void populateItem(ListItem<OrganisatieType> item)
			{
				item.add(new EnumLabel<>("organisatieType", item.getModelObject()));
				List<OrganisatieParameterKey> keys = parameterKeys.stream().filter(pk -> pk.getOrganisatieType() == item.getModelObject()).collect(Collectors.toList());
				addTabelHeader(item, keys);

				List<Instelling> instellingen = instellingService.getInstellingByOrganisatieTypes(List.of(keys.get(0).getOrganisatieType()));
				addOrganisatieLijst(item, instellingen, keys, valueFieldEnabled);
			}
		});

		addOpslaanButton(form);
	}

	protected abstract void addOpslaanButton(Form<Void> form);

	private void addTabelHeader(MarkupContainer container, List<OrganisatieParameterKey> parameterKeys)
	{
		ListView<OrganisatieParameterKey> parameters = new ListView<>("parameterHeaders", parameterKeys)
		{
			@Override
			protected void populateItem(ListItem<OrganisatieParameterKey> item)
			{
				OrganisatieParameterKey parameterKey = item.getModelObject();
				boolean parameterKeyNeedsMax = Integer.class.equals(parameterKey.getValueType());
				item.add(new EnumLabel<>("param", parameterKey));

				String maxValueTekst = String.format(getString("maxValue"), getUnitTekst(parameterKey), parameterKey.getMaxValue());
				item.add(new Label("maxValue", maxValueTekst).setVisible(parameterKeyNeedsMax));
			}

			private String getUnitTekst(OrganisatieParameterKey parameterKey)
			{

				String unit = getString(EnumStringUtil.getPropertyString(parameterKey) + ".unit");
				if (StringUtils.isNotBlank(unit))
				{
					unit += ", ";
				}
				return unit;
			}
		};
		container.add(parameters);
	}

	private void addOrganisatieLijst(MarkupContainer container, List<Instelling> instellingByOrganisatieTypes, List<OrganisatieParameterKey> parameterKeys,
		boolean valueFieldEnabled)
	{
		container.add(new ListView<>("organisaties", ModelUtil.listRModel(instellingByOrganisatieTypes, false))
		{

			@Override
			protected void populateItem(ListItem<Instelling> itemInstelling)
			{
				Instelling instelling = itemInstelling.getModelObject();
				itemInstelling.add(new Label("naam", instelling.getNaam()));

				itemInstelling.add(new ListView<>("parameterValues", parameterKeys)
				{
					@Override
					protected void populateItem(ListItem<OrganisatieParameterKey> item)
					{
						OrganisatieParameterKey parameterKey = item.getModelObject();
						voegOrganisatieParameterToeAanModel(parameterKey, itemInstelling);

						addNumberField(item, parameterKey);

						addTextField(item, parameterKey);

						addCheckboxField(item, parameterKey);

					}

					private void voegOrganisatieParameterToeAanModel(OrganisatieParameterKey parameterKey, ListItem<Instelling> itemInstelling)
					{
						Instelling instelling = itemInstelling.getModelObject();
						List<OrganisatieParameter> allParams = allParametersModel.getObject();
						allParams.add(itemInstelling.getModelObject().getParameters().stream().filter(p -> p.getKey() == parameterKey).findFirst().orElseGet(() ->
						{
							OrganisatieParameter nieuwParameter = new OrganisatieParameter();
							nieuwParameter.setOrganisatie(instelling);
							nieuwParameter.setKey(parameterKey);
							return nieuwParameter;
						}));
					}

					private void addCheckboxField(ListItem<OrganisatieParameterKey> item, OrganisatieParameterKey parameterKey)
					{
						List<OrganisatieParameter> allParams = allParametersModel.getObject();
						Class<?> valueType = parameterKey.getValueType();
						CheckBox parameterCheckbox = ComponentHelper.newCheckBox("checkbox",
							new PropertyModel<>(EditOrganisatieParametersHorizontalPanel.this, "allParameters[" + (allParams.size() - 1) + "].value"));
						parameterCheckbox.setVisible(Boolean.class.equals(valueType));
						item.add(parameterCheckbox);
					}

					private void addTextField(ListItem<OrganisatieParameterKey> item, OrganisatieParameterKey parameterKey)
					{
						List<OrganisatieParameter> allParams = allParametersModel.getObject();
						Class<?> valueType = parameterKey.getValueType();
						TextField<String> textValueField = new TextField<>("textValue",
							new PropertyModel<>(EditOrganisatieParametersHorizontalPanel.this, "allParameters[" + (allParams.size() - 1) + "].value"));
						textValueField.setVisible(String.class.equals(valueType) || BigDecimal.class.equals(valueType));
						textValueField.setEnabled(valueFieldEnabled);
						item.add(textValueField);
						if (valueType.equals(BigDecimal.class))
						{
							addBigDecimalValidator(parameterKey, textValueField);
						}
						else if (valueType.equals(String.class))
						{
							textValueField.add(StringValidator.maximumLength(255));
						}
					}

					private void addNumberField(ListItem<OrganisatieParameterKey> item, OrganisatieParameterKey parameterKey)
					{
						List<OrganisatieParameter> allParams = allParametersModel.getObject();
						Class<?> valueType = parameterKey.getValueType();
						TextField<String> numberValueField = new TextField<>("numberValue",
							new PropertyModel<>(EditOrganisatieParametersHorizontalPanel.this, "allParameters[" + (allParams.size() - 1) + "].value"));
						numberValueField.setVisible(Integer.class.equals(valueType));
						numberValueField.setEnabled(valueFieldEnabled);
						item.add(numberValueField);
						if (valueType.equals(Integer.class))
						{
							addIntegerValidators(parameterKey, numberValueField);
						}
					}

					private void addIntegerValidators(OrganisatieParameterKey parameterKey, TextField<String> valueField)
					{
						valueField.add(new StringIsNumberValidator());
						valueField.add(new RangeValidator<>(0, parameterKey.getMaxValue())
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
						valueField.add(new AttributeAppender("maxlength", Model.of(parameterKey.getMaxValue().toString().length())));
					}

					private void addBigDecimalValidator(OrganisatieParameterKey parameterKey, TextField<String> valueField)
					{
						valueField.add(new RangeValidator<>(BigDecimal.ZERO, BigDecimal.valueOf(parameterKey.getMaxValue()))
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
				});

			}

		});
	}

	public List<OrganisatieParameter> getAllParameters()
	{
		List<OrganisatieParameter> list = ModelUtil.nullSafeGet(allParametersModel);
		list.forEach(op -> op.setParameterNaam(getString(EnumStringUtil.getPropertyString(op.getKey()))));
		return list;
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(allParametersModel);
	}
}
