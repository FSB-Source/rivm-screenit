package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments;

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

import java.util.Arrays;
import java.util.Date;

import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.DocumentTemplateTestWrapper;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.enums.MergeFieldTestType;
import nl.rivm.screenit.model.mamma.enums.MammaBeperktBeoordeelbaarReden;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;

public class MergeFieldsFragment extends Fragment
{
	private static final String MARKUP_ID = "fragmentMergeFields";

	private final MarkupContainer markupProvider;

	private final MergeFieldTestType modelObject;

	public MergeFieldsFragment(final String id,
		final MarkupContainer markupProvider,
		final MergeFieldTestType modelObject,
		final IModel<DocumentTemplateTestWrapper> wrapper)
	{
		super(id, MARKUP_ID, markupProvider, wrapper);
		this.markupProvider = markupProvider;
		this.modelObject = modelObject;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(getMergeFieldListView());
	}

	private ListView<MergeField> getMergeFieldListView()
	{
		return new ListView<MergeField>("mergeFields", MergeField.getFieldWithType(modelObject))
		{
			@Override
			protected void populateItem(final ListItem<MergeField> item)
			{
				final MergeField field = item.getModelObject();
				item.add(new Label("fieldName", field.getFieldName()));
				item.add(getInvoerField(field));
			}

			private WebMarkupContainer getInvoerField(final MergeField field)
			{
				String property = "fromDB" + modelObject.name();
				String componentId = "invoerField";
				if (field.equals(MergeField.MAMMA_AFSPRAAK_BETREFT))
				{
					return new MammaAfspraakBetreftFragment(componentId,
						markupProvider,
						createModel(field),
						getEnabledModel(property));
				}
				else if (MergeFieldTestType.BKRADIOLOOG.equals(modelObject))
				{
					return new BKRadioloogFragment(componentId,
						markupProvider,
						createModel(field),
						getEnabledModel("freeText" + modelObject.name()));
				}
				else if (String.class.equals(field.getInstance()))
				{
					return new TextFragment(componentId,
						markupProvider,
						createModel(field),
						getEnabledModel(property));
				}
				else if (Integer.class.equals(field.getInstance()))
				{
					return new NumberFragment(componentId,
						markupProvider,
						createModel(field),
						getEnabledModel(property));
				}
				else if (Date.class.equals(field.getInstance()))
				{
					return new DatePickerFragment(componentId,
						markupProvider,
						createModel(field),
						getEnabledModel(property));
				}
				else if (Boolean.class.equals(field.getInstance()))
				{
					return new BooleanFragment(componentId,
						markupProvider,
						createModel(field),
						getEnabledModel(property));
				}
				else if (MammaBeperktBeoordeelbaarReden.class.equals(field.getInstance()))
				{
					return new EnumFieldsFragment<MammaBeperktBeoordeelbaarReden>(componentId,
						markupProvider,
						createModel(field),
						getEnabledModel(property),
						Arrays.asList(MammaBeperktBeoordeelbaarReden.FOTOS_MAAR_IN_1_RICHTING_GEMAAKT,
							MammaBeperktBeoordeelbaarReden.MAMMA_NIET_VOLLEDIG_AFGEBEELD));
				}
				else if (MammaVerzettenReden.class.equals(field.getInstance()))
				{
					return new MammaAfspraakRedenVerzetFragment(componentId,
						markupProvider,
						createModel(field),
						getEnabledModel(property));
				}
				return new EmptyPanel(componentId);
			}

			private IModel<Boolean> getEnabledModel(String property)
			{
				return modelObject.isFromDB() || modelObject.isFreeText()
					? new PropertyModel<>(MergeFieldsFragment.this.getDefaultModel(), property)
					: null;
			}

			private <T> IModel<T> createModel(final MergeField field)
			{
				return StringUtils.isNotBlank(field.getProperty())
					? new PropertyModel<>(MergeFieldsFragment.this.getDefaultModel(), field.getProperty())
					: new PropertyModel<>(field, "currentValue");
			}
		};
	}
}
