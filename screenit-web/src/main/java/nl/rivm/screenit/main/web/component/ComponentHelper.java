package nl.rivm.screenit.main.web.component;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Set;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.form.PostcodeField;
import nl.rivm.screenit.model.INaam;
import nl.topicuszorg.wicket.planning.web.component.DatePickerHelper;

import org.apache.wicket.Application;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.Component;
import org.apache.wicket.RuntimeConfigurationType;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.MarkupException;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.validation.validator.DateValidator;
import org.apache.wicket.validation.validator.StringValidator;
import org.joda.time.DateTime;
import org.joda.time.MutableDateTime;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker.ShowOnEnum;
import org.wicketstuff.wiquery.ui.datepicker.DatePickerYearRange;

import com.google.common.base.Joiner;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ComponentHelper
{
	private static final ThreadLocal<Integer> tabIndexCounterThreadLocal = ThreadLocal.withInitial(() -> 1);

	public static Label newLabel(String id)
	{
		return new Label(id);
	}

	public static Label newLabel(String id, PropertyModel<?> propertyModel)
	{
		return new Label(id, propertyModel);
	}

	public static <T> TextField<T> newTextField(String id, Class<T> modelType)
	{
		return newTextField(id, modelType, false);
	}

	public static <T> TextField<T> newTextField(String id, Class<T> modelType, Boolean required)
	{
		TextField<T> textField = new TextField<>(id, modelType);
		textField.setOutputMarkupId(true);
		textField.setRequired(required);
		return textField;
	}

	public static TextField<String> newTextField(String id, int maxLength, boolean required)
	{
		TextField<String> field = newTextField(id, String.class);
		field.setOutputMarkupId(true);
		field.add(StringValidator.maximumLength(maxLength));
		field.setRequired(required);
		return field;
	}

	public static FormComponent<String> addTextField(WebMarkupContainer webMarkupContainer, String fieldNaam, boolean required, int maximumLength, boolean inzien)
	{
		return addTextField(webMarkupContainer, fieldNaam, required, maximumLength, String.class, inzien);
	}

	public static <T> FormComponent<T> addTextField(WebMarkupContainer webMarkupContainer, final String fieldNaam, boolean required, int maximumLength, Class<T> type,
		boolean inzien)
	{
		return addTextField(webMarkupContainer, fieldNaam, required, maximumLength, type, inzien, false);
	}

	@SuppressWarnings("unchecked")
	public static <T> FormComponent<T> addTextField(WebMarkupContainer webMarkupContainer, final String fieldNaam, boolean required, int maximumLength, Class<T> type,
		boolean inzien, boolean tabIndex)
	{
		FormComponent<T> textField;
		if (type != null && type.equals(Date.class) && !inzien)
		{
			DatePicker<Date> newDatePicker = DatePickerHelper.newDatePicker(fieldNaam)
				.setYearRange(new DatePickerYearRange((short) -80, Short.parseShort(Integer.toString(Calendar.getInstance().get(Calendar.YEAR) + 10)))).setChangeMonth(true);

			textField = (FormComponent<T>) newDatePicker.add(DateValidator.maximum(new DateTime(9999, 12, 31, 0, 0).toDate()));
		}

		else if (type != null && !type.equals(String.class))
		{
			textField = new TextField<>(fieldNaam, type);
		}
		else
		{
			var stringTextField = new TextField<String>(fieldNaam);
			stringTextField.add(StringValidator.maximumLength(maximumLength));
			textField = (FormComponent<T>) stringTextField;
		}
		textField.setRequired(required);
		if (inzien)
		{
			textField.setEnabled(false);
		}
		webMarkupContainer.addOrReplace(textField);

		textField.setOutputMarkupId(true);

		setAutocompleteOff(textField);

		if (tabIndex)
		{
			textField.add(new AttributeAppender("tabindex", new Model<>(tabIndexCounterThreadLocal.get()), ""));
			tabIndexCounterThreadLocal.set(tabIndexCounterThreadLocal.get() + 1);
		}

		return textField;
	}

	public static TextArea<String> addTextArea(WebMarkupContainer webMarkupContainer, String id, boolean required, int maxLength, boolean inzien)
	{
		TextArea<String> textArea = new TextArea<>(id)
		{
			@Override
			public String getMarkupId()
			{
				return id;
			}
		};
		textArea.add(StringValidator.maximumLength(maxLength));
		textArea.setRequired(required);
		if (inzien)
		{
			textArea.setEnabled(false);
		}
		webMarkupContainer.addOrReplace(textArea);

		textArea.setOutputMarkupId(true);

		setAutocompleteOff(textArea);

		return textArea;
	}

	public static void setAutocompleteOff(Component component)
	{
		if (!RuntimeConfigurationType.DEVELOPMENT.equals(Application.get().getConfigurationType()))
		{
			component.add(new AttributeModifier("autocomplete", Model.of("off")));
		}
	}

	public static <T extends Enum<T>> ScreenitDropdown<T> newDropDownChoice(String id, IModel<T> model, Set<T> set, IChoiceRenderer<T> renderer)
	{
		ListModel<T> listModel = new ListModel<>(new ArrayList<>(set));
		ScreenitDropdown<T> dropdown = new ScreenitDropdown<>(id, model, listModel, renderer);
		dropdown.setOutputMarkupId(true);
		return dropdown;
	}

	public static CheckBox newCheckBox(String id)
	{
		return new CheckBox(id);
	}

	public static CheckBox newCheckBox(String id, boolean enabled)
	{
		CheckBox checkBox = new CheckBox(id);
		checkBox.setEnabled(enabled);
		return checkBox;
	}

	public static CheckBox newCheckBox(String id, IModel<Boolean> model)
	{
		return new CheckBox(id, model);
	}

	public static CheckBox newCheckBox(String id, IModel<Boolean> model, boolean enabled)
	{
		CheckBox checkBox = new CheckBox(id, model);
		checkBox.setEnabled(enabled);
		return checkBox;
	}

	public static TextArea<String> newTextArea(String id, int maxLength)
	{
		TextArea<String> textArea = new TextArea<>(id);
		textArea.add(StringValidator.maximumLength(maxLength));

		return textArea;
	}

	public static <T> ScreenitDropdown<T> newDropDownChoice(String id, IModel<List<T>> choices, IChoiceRenderer<T> choiceRenderer)
	{
		return newDropDownChoice(id, choices, choiceRenderer, false);
	}

	public static <T> ScreenitDropdown<T> newDropDownChoice(String id, IModel<List<T>> choices, IChoiceRenderer<T> choiceRenderer, boolean required)
	{
		ScreenitDropdown<T> dropdown = new ScreenitDropdown<>(id, choices);
		if (choiceRenderer != null)
		{
			dropdown.setChoiceRenderer(choiceRenderer);
		}

		dropdown.setRequired(required);
		return dropdown;
	}

	public static <T extends Enum<T>> ScreenitDropdown<T> addDropDownChoice(WebMarkupContainer webMarkupContainer, String dropDownChoiceNaam, boolean required, List<T> values,
		boolean inzien)
	{
		return addDropDownChoice(webMarkupContainer, dropDownChoiceNaam, required, values, null, inzien);
	}

	public static <T extends Enum<T>> ScreenitDropdown<T> addDropDownChoice(WebMarkupContainer webMarkupContainer, String dropDownChoiceNaam, boolean required, List<T> values,
		IModel<? extends List<T>> valuesModel, boolean inzien)
	{
		ScreenitDropdown<T> dropDownChoice;
		if (values != null)
		{
			dropDownChoice = new ScreenitDropdown<>(dropDownChoiceNaam, values);
		}
		else
		{
			dropDownChoice = new ScreenitDropdown<>(dropDownChoiceNaam, valuesModel);
		}
		dropDownChoice.setChoiceRenderer(new EnumChoiceRenderer<>(webMarkupContainer));
		dropDownChoice.setRequired(required);
		if (inzien)
		{
			dropDownChoice.setEnabled(Boolean.FALSE);
		}
		webMarkupContainer.add(dropDownChoice);

		return dropDownChoice;
	}

	public static <T extends INaam> ScreenitDropdown<T> addDropDownChoiceINaam(WebMarkupContainer webMarkupContainer, String dropDownChoiceNaam, boolean required, List<T> values,
		boolean inzien)
	{
		return addDropDownChoiceINaam(webMarkupContainer, dropDownChoiceNaam, required, values, null, inzien);
	}

	public static <T extends INaam> ScreenitDropdown<T> addDropDownChoiceINaam(WebMarkupContainer webMarkupContainer, String dropDownChoiceNaam, boolean required,
		IModel<? extends List<T>> values, boolean inzien)
	{
		return addDropDownChoiceINaam(webMarkupContainer, dropDownChoiceNaam, required, null, values, inzien);
	}

	public static <T extends INaam> ScreenitDropdown<T> addDropDownChoiceINaam(WebMarkupContainer webMarkupContainer, String dropDownChoiceNaam, boolean required, List<T> values,
		IModel<? extends List<T>> valuesModel, boolean inzien)
	{
		ScreenitDropdown<T> dropDownChoice;
		if (values != null)
		{
			dropDownChoice = new ScreenitDropdown<>(dropDownChoiceNaam, values);
		}
		else
		{
			dropDownChoice = new ScreenitDropdown<>(dropDownChoiceNaam, valuesModel);
		}
		dropDownChoice.setChoiceRenderer(new NaamChoiceRenderer<>());
		dropDownChoice.setRequired(required);
		if (inzien)
		{
			dropDownChoice.setEnabled(Boolean.FALSE);
		}
		webMarkupContainer.add(dropDownChoice);

		return dropDownChoice;
	}

	public static <T extends Enum<T>> RadioChoice<T> addRadioChoice(WebMarkupContainer webMarkupContainer, String id, Class<T> enumClass)
	{
		var radioChoice = new RadioChoice<>(id, Arrays.asList(enumClass.getEnumConstants()), new EnumChoiceRenderer<>(webMarkupContainer));
		radioChoice.setPrefix("<label class=\"radio\">");
		radioChoice.setSuffix("</label>");
		radioChoice.setRequired(true);
		webMarkupContainer.add(radioChoice);
		return radioChoice;
	}

	public static DatePicker<Date> newDatePicker(String id)
	{
		return newDatePicker(id, null);
	}

	public static DatePicker<Date> newDatePicker(String id, boolean enabled)
	{
		return newDatePicker(id, null, enabled);
	}

	public static DatePicker<Date> newDatePicker(String id, IModel<Date> model, final boolean enabled)
	{
		DatePicker<Date> datePicker = new DatePicker<>(id, model)
		{
			@Override
			public void renderHead(IHeaderResponse response)
			{
				if (enabled)
				{
					super.renderHead(response);
				}
			}
		};
		datePicker.setEnabled(enabled);
		datePicker.setType(Date.class);
		setOptions(datePicker);
		return datePicker;
	}

	public static DatePicker<Date> newDatePicker(String id, IModel<Date> model)
	{
		DatePicker<Date> datePicker = new DatePicker<>(id, model);
		datePicker.setType(Date.class);
		setOptions(datePicker);
		return datePicker;
	}

	public static DatePicker<Date> monthYearDatePicker(String id)
	{
		DatePicker<Date> datePicker = newDatePicker(id);
		datePicker.setChangeYear(true);
		datePicker.setChangeMonth(true);
		return datePicker;
	}

	public static DatePicker<Date> newYearDatePicker(String id)
	{
		DatePicker<Date> datePicker = newDatePicker(id);
		datePicker.setChangeYear(true);
		return datePicker;
	}

	public static DatePicker<Date> newYearDatePicker(String id, IModel<Date> model)
	{
		DatePicker<Date> datePicker = newDatePicker(id, model);
		datePicker.setChangeYear(true);
		return datePicker;
	}

	public static void setOptions(DatePicker<Date> datePicker)
	{
		datePicker.setShowOn(ShowOnEnum.FOCUS);
		datePicker.setButtonImageOnly(false);
		datePicker.setDateFormat("dd-mm-yy");

		datePicker.add(newDbRangeValidator());
	}

	public static DateValidator newDbRangeValidator()
	{
		MutableDateTime minimum = new MutableDateTime();
		minimum.setDate(1753, 1, 1);
		minimum.setTime(0);
		minimum.setYear(1753);
		MutableDateTime maximum = new MutableDateTime(minimum);
		maximum.setYear(9998);

		return DateValidator.range(minimum.toDate(), maximum.toDate());
	}

	public static TextField<String> newPostcodeTextField(WebMarkupContainer parent, String id, boolean required, boolean inzien)
	{
		TextField<String> postcode = new PostcodeField(id);
		postcode.setRequired(required);
		if (inzien)
		{
			postcode.setEnabled(false);
		}
		postcode.setOutputMarkupId(true);

		setAutocompleteOff(postcode);

		parent.add(postcode);
		return postcode;

	}

	public static void assertTag(Component component, ComponentTag tag, Set<String> tagNames)
	{
		boolean found = false;
		for (String tagName : tagNames)
		{
			if (tag.getName().equalsIgnoreCase(tagName))
			{
				found = true;
				break;
			}
		}

		if (!found)
		{
			throw createMarkupException(component, tag, tagNames);
		}
	}

	private static MarkupException createMarkupException(Component component, ComponentTag tag, Set<String> tagNames)
	{
		String msg = String.format("Component [%s] (path = [%s]) must be applied to a tag of type [%s], not: %s", component.getId(), component.getPath(),
			Joiner.on(',').join(tagNames), tag.toUserDebugString());

		throw new MarkupException(component.getMarkup().getMarkupResourceStream(), msg);
	}
}
