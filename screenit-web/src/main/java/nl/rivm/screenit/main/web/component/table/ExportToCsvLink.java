package nl.rivm.screenit.main.web.component.table;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.export.CSVDataExporter;
import org.apache.wicket.extensions.markup.html.repeater.data.table.export.IExportableColumn;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ResourceLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.ContentDisposition;
import org.apache.wicket.util.time.Duration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ExportToCsvLink<T extends Serializable, S> extends GenericPanel<T>
{
	private static final Logger LOG = LoggerFactory.getLogger(ExportToCsvLink.class);

	List<IExportableColumn<T, ?>> columns;

	public ExportToCsvLink(String id, final String bestandsnaam, IDataProvider<T> dataProvider, List<IColumn<T, S>> columns)
	{
		this(id, bestandsnaam, "Download lijst", dataProvider, columns);
	}

	public ExportToCsvLink(String id, final String bestandsnaam, String exportButtonTitel, IDataProvider<T> dataProvider, List<IColumn<T, S>> columns)
	{
		super(id);

		this.columns = filterAndMapToIExportableColumnList(columns);

		ResourceLink downloadLink = getResourceLink(bestandsnaam, dataProvider);
		downloadLink.add(new Label("exportButtonTitel", exportButtonTitel));
		add(downloadLink);
	}

	private ResourceLink<Object> getResourceLink(String bestandsnaam, IDataProvider<T> dataProvider) {
		return new ResourceLink<>("download", new AbstractResource()
		{
			@Override
			protected ResourceResponse newResourceResponse(Attributes attributes)
			{
				ResourceResponse resourceResponse = new ResourceResponse();
				resourceResponse.setFileName(bestandsnaam + ".csv");
				resourceResponse.setContentDisposition(ContentDisposition.ATTACHMENT);
				resourceResponse.setContentType("application/vnd.ms-excel");
				resourceResponse.setCacheDuration(Duration.NONE);
				resourceResponse.setWriteCallback(new WriteCallback()
				{

					@Override
					public void writeData(Attributes attributes)
					{
						try (OutputStream outputStream = attributes.getResponse().getOutputStream())
						{
							CSVDataExporter csvDataExporter = new CSVDataExporter()
							{
								protected String quoteValue(String value)
								{
									return value;
								}
							};
							csvDataExporter.setDelimiter(';');
							csvDataExporter.exportData(dataProvider, ExportToCsvLink.this.columns, outputStream);
						}
						catch (IOException e)
						{
							LOG.error("Fout bij het aanmaken van CSV: " + e.getMessage(), e);
							return;
						}
					}
				});

				return resourceResponse;
			}
		});
	}

	private List<IExportableColumn<T, ?>> filterAndMapToIExportableColumnList(List<IColumn<T, S>> columns) {
		return columns.stream()
				.filter(c -> c instanceof IExportableColumn)
				.map(c -> ((IExportableColumn<T, S>) c))
				.collect(Collectors.toList());
	}

}
