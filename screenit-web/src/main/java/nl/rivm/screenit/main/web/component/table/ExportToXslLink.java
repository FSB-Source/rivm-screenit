package nl.rivm.screenit.main.web.component.table;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.time.Duration;
import java.util.Iterator;
import java.util.List;

import nl.topicuszorg.csv2xls.services.Csv2xlsService;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.wicket.Component;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.export.IExportableColumn;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.basic.MultiLineLabel;
import org.apache.wicket.markup.html.link.ResourceLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.ContentDisposition;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.lang.Classes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ExportToXslLink<T extends Serializable, S> extends GenericPanel<T>
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(ExportToXslLink.class);

	@SpringBean
	private Csv2xlsService csv2xlsService;

	private ScreenitDataTable<T, S> dataTable;

	public ExportToXslLink(String id, String bestandsnaam)
	{
		this(id, bestandsnaam, "Download lijst", null);
	}

	public ExportToXslLink(String id, String bestandsnaam, ScreenitDataTable<T, S> dataTable)
	{
		this(id, bestandsnaam, "Download lijst", dataTable);
	}

	public ExportToXslLink(String id, final String bestandsnaam, String exportButtonTitel, ScreenitDataTable<T, S> dataTable)
	{
		super(id);
		this.dataTable = dataTable;

		ResourceLink downloadLink = new ResourceLink<>("download", new AbstractResource()
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected ResourceResponse newResourceResponse(Attributes attributes)
			{
				ResourceResponse resourceResponse = new ResourceResponse();
				resourceResponse.setFileName(bestandsnaam + ".xls");
				resourceResponse.setContentDisposition(ContentDisposition.ATTACHMENT);
				resourceResponse.setContentType("application/vnd.ms-excel");
				resourceResponse.setCacheDuration(Duration.ZERO);
				resourceResponse.setWriteCallback(new WriteCallback()
				{

					@Override
					public void writeData(Attributes attributes)
					{
						ByteArrayOutputStream outputStream = null;
						InputStream inputStream = null;
						OutputStream outputStream1 = attributes.getResponse().getOutputStream();

						try
						{
							String csv = getCsv();

							outputStream = new ByteArrayOutputStream();
							csv2xlsService.createXls(csv.toString(), outputStream, false, "HeaderValueYjKFLs23", "CelValueFKWsx3D", bestandsnaam, "dd-MM-yyyy", "dd-MM-yyyy HH:mm",
								false);

							inputStream = new ByteArrayInputStream(outputStream.toByteArray());
							IOUtils.copy(inputStream, outputStream1);
						}
						catch (IOException e)
						{
							LOG.error("Fout bij het aanmaken van XSL: " + e.getMessage(), e);
							return;
						}
						finally
						{
							close(outputStream);
							close(outputStream1);
							close(inputStream);
						}
					}
				});

				return resourceResponse;
			}
		});
		add(downloadLink);
		downloadLink.add(new Label("exportButtonTitel", exportButtonTitel));
	}

	private static void close(Closeable closable)
	{
		if (closable != null)
		{
			try
			{
				closable.close();
			}
			catch (IOException e)
			{
				LOG.error("Fout bij het sluiten van stream: " + e.getMessage(), e);
			}
		}
	}

	protected String getCsv() throws NullPointerException
	{
		StringBuffer csv = new StringBuffer();
		Iterator<? extends T> iterator = null;
		List<IColumn<T, S>> columns = null;
		int columnNumber = 0;

		if (dataTable != null)
		{
			iterator = dataTable.getDataProvider().iterator(-1, -1);
			columns = (List<IColumn<T, S>>) dataTable.getColumns();
		}

		if (iterator != null && columns != null)
		{
			for (IColumn<T, S> column : columns)
			{
				String headerString = "";
				if (column instanceof AbstractColumn)
				{
					AbstractColumn<T, S> propertyColumn = (AbstractColumn<T, S>) column;
					IModel<String> header = propertyColumn.getDisplayModel();
					try
					{
						headerString = header.getObject();
					}
					catch (ClassCastException cce)
					{

					}
				}
				csv.append(headerString);
				if (columnNumber < columns.size() - 1)
				{
					csv.append(",");
				}
				columnNumber++;
			}
			csv.append("\n");
			while (iterator.hasNext())
			{
				T object = iterator.next();
				columnNumber = 0;
				for (IColumn<T, S> column : columns)
				{
					String value = "";
					if (column instanceof AbstractColumn)
					{
						AbstractColumn<T, S> propertyColumn = (AbstractColumn<T, S>) column;
						Model<T> model = new Model<T>(object);
						Item<ICellPopulator<T>> item = new Item("testId", 0, model);
						propertyColumn.populateItem(item, "testId", model);
						Component labelComponent = item.get("testId");
						if (labelComponent instanceof Label)
						{
							Label label = (Label) labelComponent;
							value = leadingZeroFixed(label.getDefaultModelObjectAsString());
						}
						else if (labelComponent instanceof MultiLineLabel)
						{
							MultiLineLabel label = (MultiLineLabel) labelComponent;
							value = label.getDefaultModelObjectAsString();
						}
						else if (labelComponent instanceof EnumLabel)
						{
							EnumLabel<?> label = (EnumLabel<?>) labelComponent;
							Enum<?> enumValue = label.getModelObject();
							String property = Classes.simpleName(enumValue.getDeclaringClass()) + '.' + enumValue.name();
							value = getString(property);
						}
						else if (column instanceof IExportableColumn)
						{
							IModel dataModel = ((IExportableColumn) column).getDataModel(model);
							value = String.valueOf(dataModel.getObject());
						}
					}

					if (value.contains(","))
					{
						csv.append("\"" + StringEscapeUtils.unescapeHtml(value) + "\"");
					}
					else
					{
						csv.append(value);
					}
					if (columnNumber < columns.size() - 1)
					{
						csv.append(",");
					}
					columnNumber++;
				}
				csv.append("\n");
			}
		}

		return csv.toString();
	}

	private String leadingZeroFixed(String string)
	{
		try
		{
			char c = string.charAt(0);

			if (c == '0' && !string.contains("-") && !string.contains(":") && !string.contains(" "))
			{
				string = "'" + string + "'";
			}
		}
		catch (Exception e)
		{
			return string;
		}
		return string;
	}

	private void detachDataTable()
	{
		if (this.dataTable != null)
		{
			this.dataTable.getDataProvider().detach();
		}
	}
	public void setDataTable(ScreenitDataTable<T, S> dataTable)
	{
		this.dataTable = dataTable;
	}
}
